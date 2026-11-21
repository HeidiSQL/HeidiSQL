<?php
/**
 * Generates the functions-*.ini files with SQL function definitions, shown in HeidiSQL's
 * completion proposal, function menus and hints.
 *
 * Usage:
 *   php generate-functions.php <target> [--option=value ...]
 *
 * Targets and their sources:
 *   mysql       Help tables of several running MySQL servers, merged into one file:
 *               --servers=3300,3334:password,3333,3308,3309 [--host=127.0.0.1] [--user=root] [--pass=]
 *               Help tables contain no version info, so "since" and "until" (first version without a removed
 *               function) are derived from the server versions where a function appears. They are only as
 *               exact as the gaps between these versions. Current set: 5.5, 5.7, 8.0, 8.3, 9.6.
 *               A single server via --port=... is possible as well.
 *   mariadb     Documentation repository https://github.com/mariadb-corporation/mariadb-docs, via --docs=<dir>:
 *               git clone --depth 1 --filter=blob:none --sparse https://github.com/mariadb-corporation/mariadb-docs
 *               git -C mariadb-docs sparse-checkout set server/reference/sql-functions
 *                 server/reference/sql-statements/geometry-constructors
 *                 server/reference/sql-structure/operators/comparison-operators
 *                 server/reference/sql-structure/sequences/sequence-functions
 *                 server/server-usage/storage-engines/spider/spider-functions
 *               Alternatively the help tables of a running server via --port etc., where "since" is taken from
 *               "added in MariaDB x.y" notes. The help tables lag behind the documentation.
 *   For mysql and mariadb, exact "since" and "until" versions are then taken from the function registry in the
 *   server sources on GitHub (sql/item_create.cc, sql/lex.h etc.) of all release tags, where it knows a
 *   function. Disable with --no-registry=1.
 *   postgresql  Online documentation of all versions given in --versions (default 13-18). "since" is the
 *               first of these versions mentioning a function.
 *   sqlite      Online documentation. "since" is taken from the release history.
 *   mssql       Checkout of https://github.com/MicrosoftDocs/sql-docs, passed via --docs=<dir>.
 *               "since" is taken from the monikerRange of each page:
 *               git clone --depth 1 --filter=blob:none --sparse https://github.com/MicrosoftDocs/sql-docs
 *               git -C sql-docs sparse-checkout set docs/t-sql/functions
 *               Categories missing on overview pages are taken from the previous functions-mssql.ini, or
 *               from --old=<ini>.
 *   firebird    Online language references of --versions (default 3.0,4.0,5.0).
 *   oracle      Online SQL Language Reference of --versions (default 19,21,26). Function names and categories
 *               from the overview pages of each version, declaration and description from the function pages
 *               and their text syntax diagrams of the newest version. "since" is the first version listing a
 *               function page. Documentation 26 is server version 23 (26ai reports 23.x). Functions without
 *               parentheses like SYSDATE are skipped, as HeidiSQL inserts functions with parentheses.
 *
 * Common options:
 *   --cache=<dir>  Directory for downloaded pages, default: system temp dir
 *   --out=<file>   Output file, default: functions-<target>.ini next to this script
 *
 * Entries from supplements/<output file name> are merged into the result, replacing generated entries with the
 * same name and declaration. Use these for functions missing in the sources. Keys: name, declaration,
 * category, since, until, description. Without a declaration key, the values apply to all entries of that name.
 * A value "none" clears the generated value, e.g. a wrong "since". An entry
 * with "remove=1" removes all generated entries of that name. "replace=1" replaces them by this entry, which takes
 * missing category, description and since from them, e.g. for a wrong declaration.
 *
 * Output: UTF-8 without BOM, CRLF, typographic characters replaced by ASCII. "description" contains "\n"
 * for line breaks, which HeidiSQL converts. HeidiSQL hides entries with "since" newer than the server, and
 * entries with "until" lower or equal to the server version.
 */

declare(strict_types=1);

const NL = "\r\n";
const WRAP_WIDTH = 80;
const MAX_LINES = 40;

$args = parseArgs($argv);
$target = $args[0] ?? '';
$opt = $args['options'];
$cacheDir = $opt['cache'] ?? (sys_get_temp_dir() . DIRECTORY_SEPARATOR . 'heidisql-funcgen');
if (!is_dir($cacheDir)) {
    mkdir($cacheDir, 0777, true);
}

switch ($target) {
    case 'mysql':
        $entries = isset($opt['servers']) ? genMysqlMulti($opt) : genMysql($opt, false);
        if (empty($opt['no-registry'])) {
            $entries = applyRegistryVersions($entries, false);
        }
        break;
    case 'mariadb':
        $entries = isset($opt['docs']) ? genMariadbDocs($opt['docs']) : genMysql($opt, true);
        if (empty($opt['no-registry'])) {
            $entries = applyRegistryVersions($entries, true);
        }
        break;
    case 'postgresql':
        $entries = genPostgresql($opt);
        break;
    case 'sqlite':
        $entries = genSqlite();
        break;
    case 'mssql':
        $entries = genMssql($opt);
        break;
    case 'firebird':
        $entries = genFirebird($opt);
        break;
    case 'oracle':
        $entries = genOracle($opt);
        break;
    default:
        fwrite(STDERR, "Unknown target \"$target\". See header of " . basename(__FILE__) . " for usage.\n");
        exit(1);
}

$outFile = $opt['out'] ?? ('functions-' . $target . '.ini');
if (!preg_match('#[/\\\\]#', $outFile)) {
    $outFile = __DIR__ . DIRECTORY_SEPARATOR . $outFile;
}
$entries = mergeSupplements($entries, __DIR__ . '/supplements/' . basename($outFile));
$entries = resolvePlaceholders($entries);
file_put_contents($outFile, buildIni($entries));
echo 'Wrote ' . count($entries) . ' entries to ' . $outFile . "\n";


// ---------------------------------------------------------------------------------------------------
// Common helpers
// ---------------------------------------------------------------------------------------------------

function parseArgs(array $argv): array
{
    $result = ['options' => []];
    foreach (array_slice($argv, 1) as $arg) {
        if (preg_match('#^--([\w-]+)=(.*)$#s', $arg, $m)) {
            $result['options'][$m[1]] = $m[2];
        } else {
            $result[] = $arg;
        }
    }
    return $result;
}

function entry(string $name, string $declaration, string $category, string $description, string $since = ''): array
{
    return [
        'name' => trim($name),
        'declaration' => normalizeDeclaration($declaration),
        'category' => trim($category),
        'description' => $description,
        'since' => $since,
        'until' => '',
    ];
}

function normalizeDeclaration(string $decl): string
{
    $decl = toAscii($decl);
    $decl = preg_replace('#\s+#', ' ', $decl);
    $decl = preg_replace('#\(\s+#', '(', $decl);
    $decl = preg_replace('#\s+\)#', ')', $decl);
    return trim($decl);
}

/** Download a URL, cached. Returns null on HTTP errors like 404 */
function download(string $url, bool $quiet = false): ?string
{
    global $cacheDir;
    $file = $cacheDir . DIRECTORY_SEPARATOR . preg_replace('#[^\w.-]+#', '_', preg_replace('#^https?://#', '', $url));
    if (file_exists($file)) {
        $data = file_get_contents($file);
        return $data === '' ? null : $data;
    }
    $context = stream_context_create(['http' => ['ignore_errors' => true, 'user_agent' => 'HeidiSQL function list generator']]);
    $data = @file_get_contents($url, false, $context);
    $headers = http_get_last_response_headers() ?? [];
    $status = $headers[0] ?? '';
    if ($data === false || !preg_match('#\s200\b#', $status)) {
        if (!$quiet) {
            fwrite(STDERR, "Not found: $url ($status)\n");
        }
        file_put_contents($file, '');
        return null;
    }
    file_put_contents($file, $data);
    return $data;
}

/** Replace typographic and other non-ASCII characters by ASCII equivalents, where sensible */
function toAscii(string $s): string
{
    static $map = [
        "\u{2018}" => "'", "\u{2019}" => "'", "\u{201A}" => "'", "\u{2032}" => "'",
        "\u{201C}" => '"', "\u{201D}" => '"', "\u{201E}" => '"', "\u{2033}" => '"',
        "\u{2013}" => '-', "\u{2014}" => '-', "\u{2212}" => '-', "\u{2010}" => '-', "\u{2011}" => '-',
        "\u{2026}" => '...', "\u{2192}" => '->', "\u{2190}" => '<-', "\u{21D2}" => '=>',
        "\u{2264}" => '<=', "\u{2265}" => '>=', "\u{2260}" => '<>', "\u{00D7}" => 'x',
        "\u{00A0}" => ' ', "\u{2009}" => ' ', "\u{202F}" => ' ', "\u{2002}" => ' ', "\u{2003}" => ' ',
        "\u{200B}" => '', "\u{FEFF}" => '', "\u{00AD}" => '', "\u{2022}" => '-', "\u{00B7}" => '-',
        "\u{2713}" => 'yes', "\u{221E}" => 'infinity', "\u{03C0}" => 'pi', "\u{00B1}" => '+/-',
    ];
    return strtr($s, $map);
}

/** Clean up a text from HTML: decode entities, remove tags, normalize white space */
function htmlToText(string $html): string
{
    $html = preg_replace('#<br\s*/?>#i', ' ', $html);
    $text = strip_tags($html);
    $text = html_entity_decode($text, ENT_QUOTES | ENT_HTML5, 'UTF-8');
    $text = toAscii($text);
    return trim(preg_replace('#\s+#u', ' ', $text));
}

/**
 * Build the description from paragraphs: word wrap prose paragraphs, keep preformatted ones,
 * limit number of lines, append URL.
 * @param array $paragraphs Strings; prefix a paragraph with "\x01" to keep its line breaks
 */
function buildDescription(array $paragraphs, string $url = ''): string
{
    $lines = [];
    foreach ($paragraphs as $p) {
        if ($p === '' || $p === "\x01") {
            continue;
        }
        if ($p[0] === "\x01") {
            $pLines = explode("\n", rtrim(substr($p, 1)));
        } else {
            $pLines = explode("\n", wordwrap(trim(preg_replace('#\s+#', ' ', $p)), WRAP_WIDTH));
        }
        if ($lines) {
            $lines[] = '';
        }
        array_push($lines, ...$pLines);
    }
    if (count($lines) > MAX_LINES) {
        $lines = array_slice($lines, 0, MAX_LINES);
        $lines[] = '...';
    }
    if ($url !== '') {
        $lines[] = '';
        $lines[] = 'URL: ' . $url;
    }
    return implode("\n", array_map('rtrim', $lines));
}

function entryKey(array $e): string
{
    return strtoupper($e['name']) . '|' . strtolower(preg_replace('#\s+#', '', $e['declaration']));
}

/** Remove duplicate entries with same name and declaration, keeping the first */
function uniqueEntries(array $entries): array
{
    $result = [];
    foreach ($entries as $e) {
        $key = entryKey($e);
        if (!isset($result[$key])) {
            $result[$key] = $e;
        }
    }
    return array_values($result);
}

function mergeSupplements(array $entries, string $file): array
{
    if (!file_exists($file)) {
        return $entries;
    }
    // Own parser, as parse_ini_file treats ";" and other characters in values specially
    $sections = [];
    $section = null;
    foreach (file($file, FILE_IGNORE_NEW_LINES) as $line) {
        if (preg_match('#^\s*(;|$)#', $line)) {
            continue;
        }
        if (preg_match('#^\[(.+)\]\s*$#', $line, $m)) {
            $section = $m[1];
            $sections[$section] = [];
        } elseif ($section !== null && preg_match('#^(\w+)=(.*)$#', $line, $m)) {
            $sections[$section][strtolower($m[1])] = trim($m[2]);
        }
    }
    $entries = uniqueEntries($entries);
    foreach ($sections as $section => $values) {
        $name = $values['name'] ?? preg_replace('#\d+$#', '', $section);
        if (!empty($values['remove'])) {
            $entries = array_values(array_filter($entries, fn($e) => strcasecmp($e['name'], $name) != 0));
            continue;
        }
        if (!empty($values['replace'])) {
            // Drop generated entries of that name, but keep their values for this one, see resolvePlaceholders()
            $first = true;
            foreach ($entries as $i => $e) {
                if (strcasecmp($e['name'], $name) == 0 && empty($e['placeholder'])) {
                    if ($first) {
                        $entries[$i]['placeholder'] = true;
                        $first = false;
                    } else {
                        unset($entries[$i]);
                    }
                }
            }
            $entries = array_values($entries);
        }
        $sup = entry($name, $values['declaration'] ?? '', $values['category'] ?? '', '', $values['since'] ?? '');
        $sup['description'] = str_replace('\n', "\n", $values['description'] ?? '');
        $sup['until'] = $values['until'] ?? '';
        // Without declaration key: update all entries of that name
        $allOfName = !isset($values['declaration']);
        if ($allOfName) {
            unset($sup['declaration']);
        }
        $replaced = false;
        foreach ($entries as $i => $e) {
            if ($allOfName ? strcasecmp($e['name'], $name) == 0 : entryKey($e) === entryKey($sup)) {
                // Keep generated values where the supplement has none
                foreach ($sup as $k => $v) {
                    if ($v === 'none') {
                        $entries[$i][$k] = '';
                    } elseif ($v !== '') {
                        $entries[$i][$k] = $v;
                    }
                }
                $replaced = true;
            }
        }
        if (!$replaced) {
            $sup['declaration'] ??= '';
            $entries[] = $sup;
        }
    }
    return $entries;
}

/**
 * Entries marked "placeholder" have no usable declaration, but category, description and since from the
 * sources. They fill these values into supplement entries of the same name, or with the placeholder name as
 * prefix if that ends with "_", and are dropped then.
 */
function resolvePlaceholders(array $entries): array
{
    $placeholders = [];
    foreach ($entries as $e) {
        if (!empty($e['placeholder'])) {
            $placeholders[strtoupper($e['name'])] ??= $e;
        }
    }
    $result = [];
    foreach ($entries as $e) {
        if (!empty($e['placeholder'])) {
            continue;
        }
        $p = $placeholders[strtoupper($e['name'])] ?? null;
        // Prefix placeholder of a group page: "STATS_T_TEST_" for STATS_T_TEST_ONE
        foreach ($placeholders as $pName => $candidate) {
            if (!$p && str_ends_with($pName, '_') && str_starts_with(strtoupper($e['name']), $pName)) {
                $p = $candidate;
            }
        }
        if ($p) {
            foreach (['category', 'description', 'since'] as $k) {
                if ($e[$k] === '') {
                    $e[$k] = $p[$k];
                }
            }
        }
        $result[] = $e;
    }
    return $result;
}

function buildIni(array $entries): string
{
    $entries = uniqueEntries($entries);
    usort($entries, fn($a, $b) => [strtoupper($a['name']), $a['declaration']] <=> [strtoupper($b['name']), $b['declaration']]);
    $counts = [];
    foreach ($entries as $e) {
        $u = strtoupper($e['name']);
        $counts[$u] = ($counts[$u] ?? 0) + 1;
    }
    $numbers = [];
    $out = [];
    foreach ($entries as $e) {
        $u = strtoupper($e['name']);
        if ($counts[$u] > 1) {
            $numbers[$u] = ($numbers[$u] ?? 0) + 1;
            $section = $u . $numbers[$u];
        } else {
            $section = $u;
        }
        $descr = toAscii($e['description']);
        $descr = str_replace(["\r\n", "\r", "\n"], '\n', $descr);
        $block = '[' . $section . ']' . NL;
        if ($section !== $e['name']) {
            $block .= 'name=' . $e['name'] . NL;
        }
        $block .= 'declaration=' . $e['declaration'] . NL;
        $block .= 'category=' . $e['category'] . NL;
        if ($e['since'] !== '') {
            $block .= 'since=' . $e['since'] . NL;
        }
        if ($e['until'] !== '') {
            $block .= 'until=' . $e['until'] . NL;
        }
        $block .= 'description=' . $descr . NL;
        $out[] = $block;
    }
    return implode('', $out);
}

/** Return contents of balanced parentheses, starting at the opening parenthesis at $pos */
function balancedParens(string $s, int $pos): ?string
{
    $depth = 0;
    $len = strlen($s);
    for ($i = $pos; $i < $len; $i++) {
        if ($s[$i] === '(') {
            $depth++;
        } elseif ($s[$i] === ')') {
            $depth--;
            if ($depth === 0) {
                return substr($s, $pos + 1, $i - $pos - 1);
            }
        }
    }
    return null;
}

function compareVersions(string $a, string $b): int
{
    return version_compare($a, $b);
}


// ---------------------------------------------------------------------------------------------------
// MySQL / MariaDB: help tables
// ---------------------------------------------------------------------------------------------------

/**
 * MySQL: merge help tables of several servers, given in ascending version order:
 * --servers=3300,3334:password,3333,3308,3309 (port[:password]). Descriptions are taken from the newest server
 * having a function. "since" and "until" are derived from the first and last server version having it.
 */
function genMysqlMulti(array $opt): array
{
    $servers = [];
    foreach (explode(',', $opt['servers']) as $server) {
        [$port, $pass] = array_pad(explode(':', $server, 2), 2, $opt['pass'] ?? '');
        $version = '';
        $entries = genMysql(['port' => $port, 'pass' => $pass] + $opt, false, $version);
        $version = preg_replace('#^(\d+\.\d+\.\d+).*$#', '$1', $version);
        $byName = [];
        foreach ($entries as $e) {
            $byName[strtoupper($e['name'])][] = $e;
        }
        $servers[] = ['version' => $version, 'entries' => $byName];
    }
    usort($servers, fn($a, $b) => compareVersions($a['version'], $b['version']));
    $last = count($servers) - 1;

    $names = [];
    foreach ($servers as $s) {
        $names += array_flip(array_keys($s['entries']));
    }
    $result = [];
    foreach (array_keys($names) as $name) {
        $seen = [];
        foreach ($servers as $i => $s) {
            if (isset($s['entries'][$name])) {
                $seen[] = $i;
            }
        }
        $first = $seen[0];
        $newest = end($seen);
        $since = $first === 0 ? '' : versionBoundary($servers[$first - 1]['version'], $servers[$first]['version'], true);
        // Removed: missing in all versions after the last one having it. Gaps in between are ignored, as they are
        // mostly parsing issues of single help tables.
        $until = $newest === $last ? '' : versionBoundary($servers[$newest]['version'], $servers[$newest + 1]['version'], false);
        foreach ($servers[$newest]['entries'][$name] as $e) {
            $e['since'] = $since;
            $e['until'] = $until;
            $result[] = $e;
        }
    }
    return $result;
}

/**
 * Version where a change between two known server versions is assumed. Inclusive, so a function is rather shown
 * on a server not having it than hidden on a server having it:
 * Added between 5.7.9 and 8.0.26 => 8.0, between 8.0.26 and 8.3.0 => 8.0.27.
 * Removed between 5.7.9 and 8.0.26 => 8.0, between 8.0.26 and 8.3.0 => 8.3.0.
 */
function versionBoundary(string $before, string $after, bool $added): string
{
    $b = array_map('intval', explode('.', $before));
    $a = array_map('intval', explode('.', $after));
    if ($b[0] !== $a[0]) {
        return $a[0] . '.' . ($a[1] ?? 0);
    }
    return $added ? $b[0] . '.' . ($b[1] ?? 0) . '.' . (($b[2] ?? 0) + 1) : $after;
}

function genMysql(array $opt, bool $isMariaDB, string &$version = ''): array
{
    mysqli_report(MYSQLI_REPORT_ERROR | MYSQLI_REPORT_STRICT);
    $db = mysqli_connect($opt['host'] ?? '127.0.0.1', $opt['user'] ?? 'root', $opt['pass'] ?? '', null, (int)($opt['port'] ?? 3306));
    mysqli_set_charset($db, 'utf8mb4');
    $version = mysqli_fetch_row(mysqli_query($db, 'SELECT VERSION()'))[0];
    echo "Reading help tables from $version\n";
    $res = mysqli_query($db, "SELECT t.name, t.description, c.name AS category
        FROM mysql.help_topic t JOIN mysql.help_category c ON t.help_category_id = c.help_category_id
        ORDER BY t.name");

    // Statement and other non-function categories
    $skipCategories = '#^(Internal|Data Manipulation|Data Definition|Compound Statements|Administration|'
        . 'Account Management|Transactions|Utility|Help Metadata|Language Structure|Storage Engines|'
        . 'Components|Prepared Statements|Table Maintenance|Plugins|Replication|Data Types|Logical Operators|'
        . 'User-Defined Functions|Server Administration|Geographic Features|Functions$)#i';

    $entries = [];
    while ($row = mysqli_fetch_object($res)) {
        if (preg_match($skipCategories, $row->category)) {
            continue;
        }
        // "IF FUNCTION", "REPLACE Function", "Median Window Function", "JSON_MERGE_PATCH()"
        if (!preg_match('#^(\w+)(\(\))?(\s+(window|aggregate))?(\s+function)?$#i', $row->name, $m)) {
            continue;
        }
        $name = $m[1];
        if (preg_match('#^(IS|IN|INTERVAL|CASE|LIKE|REGEXP|BETWEEN|NOT|DIV|MOD_OPERATOR|BINARY)$#i', $name)
            && !preg_match('#^' . $name . '\s*\(#im', $row->description)) {
            continue;
        }
        $text = str_replace("\r\n", "\n", $row->description);

        // Syntax block
        if ($isMariaDB) {
            $syntax = preg_match('#Syntax\n-+\n(.*?)\n\n(Description|Examples|Return|[A-Z][\w ]+\n-+)#s', $text, $ms) ? $ms[1] : '';
        } elseif (preg_match('#^Syntax:\n(.*?)\n\n#s', $text, $ms)) {
            $syntax = $ms[1];
        } else {
            // Some topics start directly with the signature, e.g. "ROW_NUMBER() over_clause"
            $first = explode("\n\n", $text, 2)[0];
            $syntax = preg_match('#\b' . preg_quote($name, '#') . '\s*\(#i', $first) ? $first : '';
            $text = $syntax !== '' ? "Syntax:\n" . $text : $text;
        }
        $syntax = preg_replace('#\s+#', ' ', $syntax);
        $declarations = [];
        $offset = 0;
        while (preg_match('#\b' . preg_quote($name, '#') . '\s*\(#i', $syntax, $md, PREG_OFFSET_CAPTURE, $offset)) {
            $parenPos = $md[0][1] + strlen($md[0][0]) - 1;
            $decl = balancedParens($syntax, $parenPos);
            if ($decl === null) {
                break;
            }
            $declarations[] = $decl;
            $offset = $parenPos + strlen($decl) + 2;
        }
        if (!$declarations) {
            continue;
        }

        // Description body and URL
        $url = preg_match('#\nURL: (\S+)#', $text, $mu) ? $mu[1] : '';
        $body = preg_replace('#\nURL: \S+\s*$#s', '', $text);
        $since = '';
        if ($isMariaDB) {
            $since = mariadbSince($name, $body);
            $body = preg_match('#\nDescription\n-+\n(.*)$#s', $body, $mb) ? $mb[1] : $body;
            $body = preg_replace('#\n(Examples?|See Also)\n-+\n.*$#s', '', "\n" . $body);
        } else {
            $body = preg_replace('#^Syntax:\n.*?\n\n#s', '', $body);
        }
        $paragraphs = array_map(fn($p) => "\x01" . $p, preg_split('#\n{2,}#', trim($body)));
        $description = buildDescription($paragraphs, $url);

        $category = mysqlCategory($name, $row->category);
        foreach ($declarations as $decl) {
            $entries[] = entry(strtoupper($name), $decl, $category, $description, $since);
        }
    }
    return $entries;
}

// ---------------------------------------------------------------------------------------------------
// MariaDB: documentation repository https://github.com/mariadb-corporation/mariadb-docs (Markdown)
// ---------------------------------------------------------------------------------------------------

function genMariadbDocs(string $root): array
{
    $root = rtrim($root, '/\\');
    $dirs = [
        "$root/server/reference/sql-functions",
        "$root/server/reference/sql-statements/geometry-constructors",
        "$root/server/reference/sql-structure/operators/comparison-operators",
        "$root/server/reference/sql-structure/sequences/sequence-functions",
        "$root/server/server-usage/storage-engines/spider/spider-functions",
    ];
    $entries = [];
    foreach ($dirs as $dir) {
        if (!is_dir($dir)) {
            throw new RuntimeException("Directory not found: $dir. See header of " . basename(__FILE__) . ' for the checkout.');
        }
        $it = new RecursiveIteratorIterator(new RecursiveDirectoryIterator($dir, FilesystemIterator::SKIP_DOTS));
        foreach ($it as $file) {
            if ($file->getExtension() !== 'md' || in_array($file->getFilename(), ['README.md', 'SUMMARY.md'])) {
                continue;
            }
            $rel = str_replace('\\', '/', substr($file->getPathname(), strlen($root) + 1));
            array_push($entries, ...parseMariadbDocsPage(file_get_contents($file->getPathname()), $rel,
                mariadbDocsCategory($file->getPath())));
        }
    }
    return $entries;
}

/** Category from the title of the directory's README.md, e.g. "Date & Time Functions" */
function mariadbDocsCategory(string $dir): string
{
    static $cache = [];
    if (!isset($cache[$dir])) {
        $readme = @file_get_contents("$dir/README.md");
        $title = ($readme !== false && preg_match('#^\# (.+)$#m', $readme, $m)) ? trim($m[1])
            : ucwords(str_replace('-', ' ', basename($dir)));
        $title = str_replace(['&', '\\'], ['and', ''], $title);
        $cache[$dir] = mysqlCategory('', $title);
    }
    return $cache[$dir];
}

function parseMariadbDocsPage(string $md, string $relPath, string $category): array
{
    $md = str_replace("\r\n", "\n", $md);
    $front = preg_match('#^---\n(.*?)\n---\n#s', $md, $mf) ? $mf[1] : '';
    $body = substr($md, strlen($mf[0] ?? ''));
    if (!preg_match('#^\# (.+)$#m', $body, $mt)) {
        return [];
    }
    $title = str_replace('\\', '', $mt[1]);
    // Syntax: first code block in the "Syntax" section, or the first code block at all
    $syntaxSection = preg_match('#\n\#\# Syntax\s*\n(.*?)(?=\n\#\# |$)#s', $body, $ms) ? $ms[1] : $body;
    if (!preg_match('#```\w*\n(.*?)```#s', $syntaxSection, $mc)) {
        return [];
    }
    $syntaxLines = str_replace('\\', '', $mc[1]);
    $syntax = preg_replace('#\s+#', ' ', $syntaxLines);

    // Function names: words in the title, e.g. "IF Function", "ST_AsText", "CAST and CONVERT", plus synonyms
    // starting a syntax line, e.g. "AsText(g)" below "ST_AsText(g)"
    preg_match_all('#\b([A-Za-z][A-Za-z0-9_]+)\b#', $title, $mn);
    $titleWords = array_map('strtoupper', $mn[1]);
    preg_match_all('#^\s*([A-Za-z][A-Za-z0-9_]*)\s*\(#m', $syntaxLines, $ml);
    preg_match_all('#\b([A-Za-z][A-Za-z0-9_]*)\s*\(#', $syntax, $ms2);
    $names = [];
    foreach (array_merge($ms2[1], $ml[1]) as $ident) {
        $u = strtoupper($ident);
        if (in_array($u, $titleWords) || in_array($ident, $ml[1])) {
            $names[] = $u;
        }
    }
    $names = array_diff(array_unique($names), ['FUNCTION', 'OPERATOR', 'AND', 'OR', 'NOT', 'SELECT', 'AGAINST',
        'ANALYSE', 'PROCEDURE', 'CREATE', 'SET', 'VALUES', 'IN', 'FUNCTION_NAME', 'WINDOW_FUNCTION']);

    // Description section, or paragraphs after the syntax
    $descr = preg_match('#\n\#\# Description\s*\n(.*?)(?=\n\#\# |$)#s', $body, $md2) ? $md2[1] : '';
    $paragraphs = [];
    $descr = preg_replace('#```.*?```#s', '', $descr);
    foreach (preg_split('#\n\s*\n#', $descr) as $p) {
        // Skip tables and sub headings
        if (preg_match('#^\s*(\||\#)#', $p)) {
            continue;
        }
        if (preg_match('#^\s*[*-]\s+#', $p)) {
            // List: one line per item, wrapped with indentation
            $lines = [];
            foreach (preg_split('#\n\s*[*-]\s+#', "\n" . trim($p), -1, PREG_SPLIT_NO_EMPTY) as $item) {
                $item = mariadbDocsText($item);
                if ($item !== '') {
                    $lines[] = '- ' . str_replace("\n", "\n  ", wordwrap($item, WRAP_WIDTH - 2));
                }
            }
            $text = $lines ? "\x01" . implode("\n", $lines) : '';
        } else {
            $text = mariadbDocsText($p);
        }
        if ($text !== '' && count($paragraphs) < 4) {
            $paragraphs[] = $text;
        }
    }
    if (!$paragraphs && preg_match('#^description:\s*>?-?\s*(.*?)(?=\n\w+:|$)#s', $front, $mdesc)) {
        $paragraphs[] = trim(preg_replace('#\s+#', ' ', $mdesc[1]));
    }
    // Version notes in hints, e.g. "JSON_ARRAY_INTERSECT is available from MariaDB 11.2."
    $plain = preg_replace('#\[([^\]]*)\]\([^)]*\)#', '$1', $body);
    $since = preg_match('#(?:available|introduced|added)\s+(?:from|in)\s+MariaDB\s+(?:Community Server\s+)?(\d+\.\d+(?:\.\d+)?)#i', $plain, $mv)
        ? $mv[1] : '';
    $url = 'https://mariadb.com/docs/' . preg_replace('#\.md$#', '', $relPath);
    $description = buildDescription($paragraphs, $url);

    $entries = [];
    foreach ($names as $name) {
        $offset = 0;
        while (preg_match('#\b' . preg_quote($name, '#') . '\s*\(#i', $syntax, $mf2, PREG_OFFSET_CAPTURE, $offset)) {
            $parenPos = $mf2[0][1] + strlen($mf2[0][0]) - 1;
            $decl = balancedParens($syntax, $parenPos);
            if ($decl === null) {
                break;
            }
            $entries[] = entry($name, $decl, mysqlCategory($name, $category), $description, $since);
            $offset = $parenPos + strlen($decl) + 2;
        }
    }
    return $entries;
}

/** Markdown and GitBook blocks to plain text */
function mariadbDocsText(string $s): string
{
    $s = preg_replace('#\{%[^%]*%\}#', '', $s);
    $s = preg_replace('#<[^>]+>#', '', $s);
    $s = str_replace('\\', '', $s);
    return markdownToText($s);
}

// ---------------------------------------------------------------------------------------------------
// MySQL / MariaDB: version history of the native function registry in the server sources
// ---------------------------------------------------------------------------------------------------

/**
 * Sources declaring function names, per repository. Missing files in older tags are skipped.
 */
function registrySources(bool $isMariaDB): array
{
    return $isMariaDB
        ? ['repo' => 'MariaDB/server', 'prefix' => 'mariadb-', 'min' => '10.0.10',
            'files' => ['sql/item_create.cc', 'sql/item_geofunc.cc', 'sql/lex.h', 'sql/sql_type_geom.cc',
                'plugin/type_inet/plugin.cc', 'plugin/type_uuid/plugin.cc']]
        : ['repo' => 'mysql/mysql-server', 'prefix' => 'mysql-', 'min' => '5.5.8',
            'files' => ['sql/item_create.cc', 'sql/lex.h']];
}

/**
 * Function names declared in one source file.
 * @return array name => "fn" for registered functions, "kw" for plain keywords
 */
function registryNamesFromSource(string $src): array
{
    $result = [];
    // lex.h: all keywords, as grammar defined functions like COALESCE, IF or REPLACE are plain keywords. Only
    // names of functions from the help are looked up, so other keywords don't matter. Keywords are not used
    // to detect removed functions, as for example PASSWORD stays a keyword after PASSWORD() was removed.
    // MySQL 8+: {SYM("COALESCE", COALESCE)}, MariaDB and MySQL 5.x: { "COALESCE", SYM(COALESCE)},
    preg_match_all('#\bSYM(?:_HK|_H)?\("([A-Z][A-Z0-9_]*)"#', $src, $m);
    foreach ($m[1] as $name) {
        $result[$name] = 'kw';
    }
    preg_match_all('#\{\s*"([A-Z][A-Z0-9_]*)"\s*,\s*SYM#', $src, $m);
    foreach ($m[1] as $name) {
        $result[$name] = 'kw';
    }
    // lex.h, functions: MySQL 8+ {SYM_FN("ADDDATE", ADDDATE_SYM)}, MariaDB and MySQL 5.x: sql_functions[] array
    $names = [];
    preg_match_all('#SYM_FN\("([A-Z][A-Z0-9_]*)"#', $src, $m);
    array_push($names, ...$m[1]);
    if (preg_match('#sql_functions\[\]\s*=\s*\{(.*?)\n\};#s', $src, $mb)) {
        preg_match_all('#\{\s*"([A-Z][A-Z0-9_]*)"\s*,#', $mb[1], $m);
        array_push($names, ...$m[1]);
    }
    // MariaDB and MySQL 5.x: { { STRING_WITH_LEN("ABS") }, BUILDER(Create_func_abs)},
    preg_match_all('#\{\s*\{\s*(?:C_)?STRING_WITH_LEN\("([A-Z][A-Z0-9_]*)"\)\s*\}\s*,\s*(?:GEOM_)?BUILDER#', $src, $m);
    array_push($names, ...$m[1]);
    // MySQL 8+: {"ABS", SQL_FN(Item_func_abs, 1)},
    preg_match_all('#\{"([A-Z][A-Z0-9_]*)",\s*SQL_#', $src, $m);
    array_push($names, ...$m[1]);
    // MariaDB 10.5+, geometry constructors POINT() etc.: Named_type_handler<Type_handler_point> type_handler_point("point");
    preg_match_all('#Named_type_handler<\w+>\s+\w+\("([a-z][a-z0-9_]*)"\)#', $src, $m);
    array_push($names, ...array_map('strtoupper', $m[1]));
    // Function plugins: "Function INET6_ATON()",
    preg_match_all('#"Function ([A-Za-z][A-Za-z0-9_]*)\(\)"#', $src, $m);
    array_push($names, ...array_map('strtoupper', $m[1]));
    foreach ($names as $name) {
        $result[$name] = 'fn';
    }
    return $result;
}

/** Release tags of a repository, sorted by version */
function githubReleaseTags(string $repo, string $prefix, string $minVersion): array
{
    $json = download("https://api.github.com/repos/$repo/git/matching-refs/tags/$prefix")
        ?? throw new RuntimeException("Could not read tags of $repo");
    $versions = [];
    foreach (json_decode($json, true) as $ref) {
        if (preg_match('#^refs/tags/' . preg_quote($prefix, '#') . '(\d+\.\d+\.\d+)$#', $ref['ref'], $m)
            && compareVersions($m[1], $minVersion) >= 0) {
            $versions[] = $m[1];
        }
    }
    usort($versions, 'compareVersions');
    return $versions;
}

/**
 * Names in the function registry for each release tag. Only the first and last tag of each major.minor series
 * are read, plus tags found by bisecting where these differ. Tags in between equal neighbours inherit their set.
 * @return array [versions, sets]: sets[i] = name => true
 */
function registryHistory(bool $isMariaDB): array
{
    $cfg = registrySources($isMariaDB);
    $versions = githubReleaseTags($cfg['repo'], $cfg['prefix'], $cfg['min']);
    $sets = [];
    $fetch = function (int $i) use (&$sets, $versions, $cfg) {
        if (!isset($sets[$i])) {
            $names = [];
            foreach ($cfg['files'] as $file) {
                $src = download("https://raw.githubusercontent.com/{$cfg['repo']}/{$cfg['prefix']}{$versions[$i]}/$file", true);
                if ($src !== null) {
                    foreach (registryNamesFromSource($src) as $name => $kind) {
                        if (($names[$name] ?? '') !== 'fn') {
                            $names[$name] = $kind;
                        }
                    }
                }
            }
            ksort($names);
            $sets[$i] = $names;
        }
        return $sets[$i];
    };
    $refine = function (int $a, int $b) use (&$refine, &$sets, $fetch) {
        $fetch($a);
        if ($b - $a < 1) {
            return;
        }
        if ($fetch($a) == $fetch($b)) {
            for ($i = $a + 1; $i < $b; $i++) {
                $sets[$i] ??= $sets[$a];
            }
            return;
        }
        if ($b - $a === 1) {
            return;
        }
        $m = intdiv($a + $b, 2);
        $refine($a, $m);
        $refine($m, $b);
    };
    // Series boundaries
    $series = [];
    foreach ($versions as $i => $v) {
        $series[preg_replace('#\.\d+$#', '', $v)][] = $i;
    }
    foreach ($series as $indexes) {
        $refine(reset($indexes), end($indexes));
    }
    ksort($sets);
    echo 'Read function registry of ' . count(array_unique(array_map('serialize', $sets))) . ' distinct states in '
        . count($versions) . " release tags\n";
    return [$versions, $sets];
}

/**
 * Overwrite "since" with the first release where the server sources know a name (function or keyword).
 * "until": first release after the last one knowing the name at all, if it is missing in the newest release.
 * If the newest release registers it as function, "until" is cleared. If the newest release only has it as
 * keyword (PASSWORD after PASSWORD() was removed, or ROW_NUMBER moved between arrays), the existing value is kept.
 */
function applyRegistryVersions(array $entries, bool $isMariaDB): array
{
    [$versions, $sets] = registryHistory($isMariaDB);
    $last = count($versions) - 1;
    $first = [];
    $lastSeen = [];
    foreach ($sets as $i => $set) {
        foreach ($set as $name => $kind) {
            $first[$name] ??= $i;
            $lastSeen[$name] = $i;
        }
    }
    foreach ($entries as &$e) {
        $u = strtoupper($e['name']);
        if (!isset($first[$u])) {
            continue;
        }
        $since = $first[$u] === 0 ? '' : $versions[$first[$u]];
        // A keyword can be older than the function, e.g. TO_NUMBER in Oracle mode. Keep a later version from the
        // MariaDB documentation then. For MySQL, an existing "since" is derived from help tables only, which is
        // less reliable than the registry.
        $firstIsKeyword = $sets[$first[$u]][$u] === 'kw';
        if (!$isMariaDB || !$firstIsKeyword || $e['since'] === '' || compareVersions($e['since'], $since) < 0) {
            $e['since'] = $since;
        }
        if ($lastSeen[$u] < $last) {
            $e['until'] = $versions[$lastSeen[$u] + 1];
        } elseif ($sets[$last][$u] === 'fn') {
            $e['until'] = '';
        }
    }
    return $entries;
}

/** Help categories are partly wrong in MySQL, e.g. JSON functions in "MBR Functions" */
function mysqlCategory(string $name, string $category): string
{
    $u = strtoupper($name);
    if (str_starts_with($u, 'JSON_')) {
        return 'JSON Functions';
    }
    if (in_array($u, ['STRING_TO_VECTOR', 'VECTOR_DIM', 'VECTOR_TO_STRING', 'DISTANCE']) || str_starts_with($u, 'VEC_')) {
        return 'Vector Functions';
    }
    if ($category === 'Functions and Modifiers for Use with GROUP BY') {
        return 'Aggregate Functions and Modifiers';
    }
    // Unify case, e.g. "Comparison operators" in older versions, but keep "MBR", "WKT", "GeoJSON"
    $words = explode(' ', $category);
    foreach ($words as $i => $w) {
        $words[$i] = ($i > 0 && in_array(strtolower($w), ['and', 'or', 'of', 'for', 'with', 'by', 'in']))
            ? strtolower($w) : ucfirst($w);
    }
    return implode(' ', $words);
}

/** Find "added in MariaDB x.y.z" notes in the head of the help text, or in a sentence naming the function */
function mariadbSince(string $name, string $text): string
{
    $head = preg_match('#^(.*?)\nDescription\n-+\n#s', $text, $m) ? $m[1] : '';
    $versions = [];
    $sentences = preg_split('#(?<=[.!?])\s+#', $head . "\n" . substr($text, strlen($head), 1500));
    foreach ($sentences as $sentence) {
        if (!preg_match('#\b(added|introduced)\s+in\s+MariaDB\s+(\d+\.\d+(\.\d+)?)#i', $sentence, $mv)) {
            continue;
        }
        $inHead = str_contains($head, $sentence);
        $namesFunction = preg_match('#\b' . preg_quote($name, '#') . '\b(\s*\(\))?\s+(function\s+)?(was|is|were)\b#i', $sentence)
            || preg_match('#^(The\s+)?' . preg_quote($name, '#') . '\b#i', $sentence);
        // Skip notes about clauses, options or behavior changes
        if (preg_match('#\b(clause|option|argument|parameter|keyword|syntax|support for|ability|behavio)#i', $sentence)) {
            continue;
        }
        if ($inHead || $namesFunction) {
            // Backports: "introduced in MariaDB 10.2.38, MariaDB 10.3.29, ..." - take all versions
            preg_match_all('#MariaDB\s+(\d+\.\d+(\.\d+)?)#', $sentence, $all);
            array_push($versions, ...$all[1]);
        }
    }
    if (!$versions) {
        return '';
    }
    usort($versions, 'compareVersions');
    // Versions below 10.0 are irrelevant
    return compareVersions($versions[0], '10.0') < 0 ? '' : $versions[0];
}


// ---------------------------------------------------------------------------------------------------
// PostgreSQL: online documentation, function tables since v13
// ---------------------------------------------------------------------------------------------------

function genPostgresql(array $opt): array
{
    $versions = explode(',', $opt['versions'] ?? '13,14,15,16,17,18');
    $pages = [
        'math' => 'Mathematical Functions',
        'string' => 'String Functions',
        'binarystring' => 'Binary String Functions',
        'bitstring' => 'Bit String Functions',
        'formatting' => 'Data Type Formatting Functions',
        'datetime' => 'Date/Time Functions',
        'enum' => 'Enum Support Functions',
        'geometry' => 'Geometric Functions',
        'net' => 'Network Address Functions',
        'textsearch' => 'Text Search Functions',
        'uuid' => 'UUID Functions',
        'json' => 'JSON Functions',
        'sequence' => 'Sequence Manipulation Functions',
        'comparison' => 'Comparison Functions',
        'array' => 'Array Functions',
        'range' => 'Range Functions',
        'aggregate' => 'Aggregate Functions',
        'window' => 'Window Functions',
        'merge-support' => 'Merge Support Functions',
        'srf' => 'Set Returning Functions',
        'info' => 'Session Information Functions',
        'admin' => 'System Administration Functions',
        'trigger' => 'Trigger Functions',
        'event-triggers' => 'Event Trigger Functions',
        'statistics' => 'Statistics Information Functions',
        'conditional' => 'Conditional Expressions',
        'xml' => 'XML Functions',
    ];
    $firstSeen = [];
    $entries = [];
    $newest = end($versions);
    foreach ($versions as $version) {
        foreach ($pages as $page => $category) {
            $url = "https://www.postgresql.org/docs/$version/functions-$page.html";
            $html = download($url);
            if ($html === null) {
                continue;
            }
            // Older versions describe some functions in prose instead of tables, so take any mention
            preg_match_all('#<code class="function">(\w+)</code>#', $html, $mentioned);
            foreach ($mentioned[1] as $name) {
                $firstSeen[strtoupper($name)] ??= $version;
            }
            $tableEntries = parsePgFunctionTable($html);
            $tableNames = array_flip(array_map(fn($e) => $e['name'], $tableEntries));
            $proseEntries = array_filter(parsePgSynopsis($html), fn($e) => !isset($tableNames[$e['name']]));
            foreach (array_merge($tableEntries, $proseEntries) as $e) {
                $firstSeen[strtoupper($e['name'])] ??= $version;
                if ($version === $newest) {
                    $e['category'] = $category;
                    $e['description'] = buildDescription($e['paragraphs'], "https://www.postgresql.org/docs/current/functions-$page.html");
                    unset($e['paragraphs']);
                    $entries[] = $e;
                }
            }
        }
    }
    foreach ($entries as &$e) {
        $first = $firstSeen[strtoupper($e['name'])];
        $e['since'] = $first === $versions[0] ? '' : $first;
    }
    return $entries;
}

/** Functions described in prose: <pre class="synopsis"> blocks followed by paragraphs, e.g. XML functions */
function parsePgSynopsis(string $html): array
{
    $result = [];
    preg_match_all('#<pre class="synopsis">(.*?)</pre>(.*?)(?=<pre|<div class="sect|</div>\s*<div class="sect|$)#s', $html, $ms, PREG_SET_ORDER);
    foreach ($ms as $m) {
        $synopsis = html_entity_decode(strip_tags($m[1]), ENT_QUOTES | ENT_HTML5, 'UTF-8');
        preg_match_all('#<p>(.*?)</p>#s', $m[2], $mp);
        $paragraphs = [];
        foreach ($mp[1] as $p) {
            $text = htmlToText($p);
            if ($text !== '' && !preg_match('#^Examples?:?$#i', $text) && count($paragraphs) < 3) {
                $paragraphs[] = $text;
            }
        }
        // One or more signatures, possibly spanning lines
        $synopsis = preg_replace('#\s+#', ' ', $synopsis);
        $offset = 0;
        while (preg_match('#(?<![\w.])([A-Za-z_]\w*)\s*\(#', $synopsis, $mf, PREG_OFFSET_CAPTURE, $offset)) {
            $name = $mf[1][0];
            $parenPos = $mf[0][1] + strlen($mf[0][0]) - 1;
            $decl = balancedParens($synopsis, $parenPos);
            if ($decl === null) {
                break;
            }
            $offset = $parenPos + strlen($decl) + 2;
            if (preg_match('#^(CASE|WHEN|THEN|ELSE|SELECT|FROM|AS|PASSING|COLUMNS|NAME|XMLATTRIBUTES)$#i', $name)) {
                continue;
            }
            $descr = $paragraphs;
            if (preg_match('#^\s*\x{2192}\s*(\w[\w ]*)#u', substr($synopsis, $offset), $mr)) {
                array_unshift($descr, 'Returns: ' . trim($mr[1]));
            }
            $e = entry(strtoupper($name), $decl, '', '');
            $e['paragraphs'] = $descr;
            $result[] = $e;
        }
    }
    return $result;
}

/** Parse <td class="func_table_entry"> cells: one or more signatures, description, examples */
function parsePgFunctionTable(string $html): array
{
    $result = [];
    preg_match_all('#<td\b[^>]*\bclass="func_table_entry"[^>]*>(.*?)</td>#s', $html, $cells);
    foreach ($cells[1] as $cell) {
        preg_match_all('#<p\b([^>]*)>(.*?)</p>#s', $cell, $ps, PREG_SET_ORDER);
        $signatures = [];
        $paragraphs = [];
        $examples = [];
        foreach ($ps as $p) {
            if (str_contains($p[1], 'func_signature')) {
                $signatures[] = $p[2];
            } elseif (preg_match('#^\s*<code class="literal">#', $p[2]) && str_contains($p[2], "\u{2192}")) {
                $example = htmlToText($p[2]);
                // Skip examples with a result table, which is not part of the paragraph
                if (!str_ends_with($example, '->')) {
                    $examples[] = $example;
                }
            } elseif (preg_match('#^\s*<code class="literal">[^<]*</code>\s*$#', $p[2])) {
                // Example setup statements, e.g. "create type twoints as ..."
                continue;
            } else {
                $paragraphs[] = htmlToText($p[2]);
            }
        }
        $returnTypes = [];
        $parsed = [];
        foreach ($signatures as $sig) {
            if (!preg_match('#<code class="function">(\w+)</code>\s*\(#', $sig, $m, PREG_OFFSET_CAPTURE)) {
                continue;
            }
            $name = $m[1][0];
            $text = html_entity_decode(strip_tags($sig), ENT_QUOTES | ENT_HTML5, 'UTF-8');
            $pos = strpos($text, '(', strpos($text, $name));
            $decl = balancedParens($text, $pos);
            if ($decl === null) {
                continue;
            }
            $ret = preg_match('#\x{2192}\s*(.+)$#u', $text, $mr) ? trim(preg_replace('#\s+#', ' ', $mr[1])) : '';
            $parsed[] = [$name, $decl, $ret];
        }
        foreach ($parsed as [$name, $decl, $ret]) {
            $descr = [];
            if ($ret !== '') {
                $descr[] = 'Returns: ' . toAscii($ret);
            }
            array_push($descr, ...array_slice($paragraphs, 0, 3));
            if ($examples) {
                $descr[] = "\x01Examples:\n" . implode("\n", array_slice($examples, 0, 3));
            }
            $e = entry(strtoupper($name), $decl, '', '');
            $e['paragraphs'] = $descr;
            $result[] = $e;
        }
    }
    return $result;
}


// ---------------------------------------------------------------------------------------------------
// SQLite: online documentation
// ---------------------------------------------------------------------------------------------------

function genSqlite(): array
{
    $pages = [
        'lang_corefunc' => 'Scalar Functions',
        'lang_aggfunc' => 'Aggregate Functions',
        'lang_mathfunc' => 'Mathematical Functions',
        'windowfunctions' => 'Window Functions',
    ];
    $entries = [];
    foreach ($pages as $page => $category) {
        $url = "https://www.sqlite.org/$page.html";
        $html = download($url) ?? throw new RuntimeException("Could not read $url");
        // <a name="x"></a><dt><p><b>abs(<i>X</i>)<br>...</b></dt><dd><p>text</p><p>text</dd>
        preg_match_all('#(?:<a name="(\w+)"></a>\s*)?<dt>(.*?)</dt>\s*<dd>(.*?)</dd>#s', $html, $ms, PREG_SET_ORDER);
        foreach ($ms as $m) {
            $paragraphs = array_map('htmlToText', preg_split('#<p>#', $m[3]));
            $anchor = $m[1] !== '' ? "$url#{$m[1]}" : $url;
            $description = buildDescription($paragraphs, $anchor);
            foreach (preg_split('#<br\s*/?>#', $m[2]) as $def) {
                $def = htmlToText($def);
                if (preg_match('#^(\w+)\s*\(#', $def, $md)) {
                    $decl = balancedParens($def, strpos($def, '('));
                    $entries[] = entry(strtoupper($md[1]), $decl ?? '', $category, $description);
                }
            }
        }
    }

    // Date and time functions: signatures in a list, description in sections
    $url = 'https://www.sqlite.org/lang_datefunc.html';
    $html = download($url) ?? throw new RuntimeException("Could not read $url");
    $common = 'All date and time functions take a time value as an argument, followed by zero or more modifiers. '
        . 'The time value can be a string like "YYYY-MM-DD HH:MM:SS", a Julian day number, or "now".';
    // <li> <b>unixepoch(</b><i>time-value, modifier, ...</i><b>)</b> </li>
    preg_match_all('#<li>\s*<b>(\w+)\(</b><i>(.*?)</i><b>\)</b>\s*</li>#s', $html, $ms, PREG_SET_ORDER);
    foreach ($ms as $m) {
        // "The <b>date()</b> function returns ..." or "The <b>timediff(A,B)</b> function ..."
        $text = preg_match('#<p>\s*(The <b>' . $m[1] . '\(.*?)(?=</p>|<a name=)#s', $html, $mp) ? htmlToText($mp[1]) : '';
        $entries[] = entry(strtoupper($m[1]), htmlToText($m[2]), 'Date and Time Functions',
            buildDescription([$text, $common], $url));
    }

    // JSON functions: signature list with anchors, description in the anchored section
    $url = 'https://www.sqlite.org/json1.html';
    $html = download($url) ?? throw new RuntimeException("Could not read $url");
    preg_match_all('#<li value=\'\d+\'>\s*(.*?)\s*</li>#s', $html, $items);
    foreach ($items[1] as $item) {
        foreach (preg_split('#<br\s*/?>#', $item) as $def) {
            if (!preg_match('#<a href=\'\#(\w+)\'>(\w+)</a>\s*\((.*?)\)\s*$#s', trim($def), $md)) {
                continue;
            }
            $section = '';
            if (preg_match('#<a name="' . $md[1] . '"></a>.*?</h2>(.*?)<h2#s', $html, $mSec)) {
                preg_match_all('#<p>(.*?)(?=<p>|</p>|$)#s', $mSec[1], $mp);
                $section = implode("\n\n", array_slice(array_filter(array_map('htmlToText', $mp[1])), 0, 3));
            }
            $name = strtoupper($md[2]);
            $category = preg_match('#_(each|tree)$#i', $name) ? 'JSON Table-Valued Functions'
                : (preg_match('#_group_#i', $name) ? 'JSON Aggregate Functions' : 'JSON Functions');
            $entries[] = entry($name, htmlToText($md[3]), $category,
                buildDescription(preg_split('#\n\n#', $section), "$url#{$md[1]}"));
        }
    }

    $entries = uniqueEntries($entries);
    $since = sqliteSinceMap(array_unique(array_map(fn($e) => strtolower($e['name']), $entries)));
    foreach ($entries as &$e) {
        $e['since'] = $since[strtolower($e['name'])] ?? '';
    }
    return $entries;
}

/** Earliest release whose change log item adds a function, e.g. "Added the concat() function" */
function sqliteSinceMap(array $names): array
{
    $html = download('https://www.sqlite.org/changes.html') ?? '';
    $nameSet = array_flip($names);
    $since = [];
    // Releases are listed newest first
    preg_match_all('#<h3>[^<]*\((3\.\d+\.\d+)\)</h3>(.*?)(?=<h3>|$)#s', $html, $releases, PREG_SET_ORDER);
    foreach ($releases as [, $version, $body]) {
        preg_match_all('#<li>(.*?)(?=<li>|</ol>|</ul>)#s', $body, $items);
        foreach ($items[1] as $item) {
            $text = htmlToText($item);
            if (!preg_match('#\b(add|added|adds|new)\b#i', $text)
                || preg_match('#\b(enhance|enhanced|fix|fixed|bug|option|argument|parameter)\b#i', $text)) {
                continue;
            }
            preg_match_all('#\b(\w+)\(\)#', $text, $fm);
            foreach ($fm[1] as $f) {
                $f = strtolower($f);
                if (isset($nameSet[$f]) && (!isset($since[$f]) || compareVersions($version, $since[$f]) < 0)) {
                    $since[$f] = $version;
                }
            }
        }
    }
    // Irrelevant for the SQLite libraries used in HeidiSQL
    return array_filter($since, fn($v) => compareVersions($v, '3.25.0') >= 0);
}


// ---------------------------------------------------------------------------------------------------
// MS SQL: MicrosoftDocs/sql-docs repository
// ---------------------------------------------------------------------------------------------------

function genMssql(array $opt): array
{
    $dir = rtrim($opt['docs'] ?? '', '/\\') . '/docs/t-sql/functions';
    if (!is_dir($dir)) {
        throw new RuntimeException("Directory not found: $dir. Pass the sql-docs checkout with --docs=<dir>");
    }
    $files = glob("$dir/*.md");

    // Categories from overview pages, which link the function pages
    $categories = [];
    foreach ($files as $file) {
        $md = file_get_contents($file);
        if (preg_match('#^title:\s*"?(.+? [Ff]unctions)\b#m', $md, $mt) && !preg_match('#```syntaxsql#', $md)
            && !preg_match('#^(functions|what-are)#', basename($file))) {
            $category = mssqlCategory(preg_replace('#\s*\(Transact-SQL\)#', '', $mt[1]));
            preg_match_all('#\]\(([\w-]+\.md)\)#', $md, $ml);
            foreach ($ml[1] as $linked) {
                $categories[$linked] ??= $category;
            }
        }
    }
    // Fallback: categories of the previous output file, as some overview pages are elsewhere in the docs
    $oldCategories = [];
    $oldFile = $opt['old'] ?? (__DIR__ . '/functions-mssql.ini');
    if (file_exists($oldFile)) {
        $section = null;
        foreach (file($oldFile, FILE_IGNORE_NEW_LINES) as $line) {
            if (preg_match('#^\[(.+)\]#', $line, $m)) {
                $section = preg_replace('#\d+$#', '', $m[1]);
            } elseif ($section !== null && preg_match('#^category=(.+)$#', $line, $m)) {
                $oldCategories[strtoupper($section)] = mssqlCategory(trim($m[1]));
            }
        }
    }

    $entries = [];
    foreach ($files as $file) {
        $md = str_replace("\r\n", "\n", file_get_contents($file));
        if (!preg_match_all('#```syntaxsql[ \t]*\n(.*?)```#s', $md, $ms)) {
            continue;
        }
        if (!preg_match('#^monikerRange:\s*"?([^"\n]*)#m', $md, $mm)) {
            $moniker = '';
        } else {
            $moniker = $mm[1];
        }
        $since = mssqlSince($moniker);
        if ($since === null) {
            // Not available in SQL Server or Azure SQL Database, e.g. Fabric or Synapse only
            continue;
        }
        $title = preg_match('#^title:\s*"?([^"\n]+)#m', $md, $mt) ? $mt[1] : '';
        $summary = preg_match('#^description:\s*"?([^"\n]+)#m', $md, $md2) ? trim($md2[1], '" ') : '';

        // Function names: from the title, e.g. "CAST and CONVERT (Transact-SQL)", "Logical functions - GREATEST"
        $title = preg_replace('#\s*\(Transact-SQL\)#', '', $title);
        $title = preg_replace('#^.* - #', '', $title);
        preg_match_all('#\b([A-Z][A-Z0-9_$]*[A-Z0-9_$])\b#', $title, $mn);
        $names = array_unique($mn[1]);

        $syntax = preg_replace('#--.*$#m', '', implode("\n", $ms[1]));
        $syntax = preg_replace('#\s+#', ' ', $syntax);
        $body = mssqlBody($md);
        $returns = mssqlSection($md, 'Return types?');
        $paragraphs = [];
        if ($returns !== '') {
            $paragraphs[] = preg_match('#^(Returns|The return)#i', $returns) ? $returns : 'Returns: ' . $returns;
        }
        $paragraphs = array_merge($paragraphs, $body);
        if ($summary !== '' && !$body) {
            $paragraphs[] = $summary;
        }
        $url = 'https://learn.microsoft.com/en-us/sql/t-sql/functions/' . basename($file, '.md');
        $description = buildDescription($paragraphs, $url);
        foreach ($names as $name) {
            if (in_array($name, ['AND', 'OR', 'SQL', 'JSON', 'XML', 'NULL', 'IS', 'NOT', 'ODBC'])) {
                continue;
            }
            $category = $categories[basename($file)] ?? $oldCategories[$name] ?? match (true) {
                (bool)preg_match('#^(REGEXP_|EDIT_DISTANCE|JARO_WINKLER|BASE64_|UNISTR)#', $name) => 'String Functions',
                str_starts_with($name, 'JSON_') => 'JSON Functions',
                in_array($name, ['GREATEST', 'LEAST']) => 'Logical Functions',
                default => 'Other Functions',
            };
            $offset = 0;
            while (preg_match('#\b' . preg_quote($name, '#') . '\s*\(#i', $syntax, $mf, PREG_OFFSET_CAPTURE, $offset)) {
                $parenPos = $mf[0][1] + strlen($mf[0][0]) - 1;
                $decl = balancedParens($syntax, $parenPos);
                if ($decl === null) {
                    break;
                }
                $entries[] = entry($name, $decl, $category, $description, $since);
                $offset = $parenPos + strlen($decl) + 2;
            }
        }
    }
    return $entries;
}

/**
 * monikerRange => since:
 * ">=sql-server-ver16 || ..." => 16, ">=sql-server-2016/2017 || ..." => "" (all versions), "=azuresqldb-current"
 * only => 99 (HeidiSQL ignores "since" on Azure, which reports an old version), Fabric/Synapse only => null
 */
function mssqlSince(string $moniker): ?string
{
    static $years = ['2016' => '13', '2017' => '14', '2019' => '15', '2022' => '16', '2025' => '17'];
    if ($moniker === '') {
        return '';
    }
    $versions = [];
    if (preg_match_all('#sql-server-(?:linux-)?ver(\d+)#', $moniker, $m)) {
        array_push($versions, ...$m[1]);
    }
    if (preg_match_all('#sql-server-(?:linux-)?(20\d\d)#', $moniker, $m)) {
        foreach ($m[1] as $year) {
            $versions[] = $years[$year] ?? '13';
        }
    }
    if ($versions) {
        sort($versions, SORT_NUMERIC);
        // 2016 and 2017 are the oldest monikers in the current docs, meaning "all versions"
        return $versions[0] <= 14 ? '' : $versions[0];
    }
    if (str_contains($moniker, 'azuresqldb-current')) {
        // Azure SQL only: hide on boxed servers via a far future version
        return '99';
    }
    return null;
}

/** Unify category names: "String" => "String Functions", "Date and Time Data Types and Functions" => ... */
function mssqlCategory(string $c): string
{
    $c = trim(preg_replace('#^(What are |Transact-SQL |The )#i', '', trim($c, ' "')));
    if (preg_match('#^Date and time#i', $c)) {
        return 'Date and Time Functions';
    }
    $c = preg_replace('#\s+functions$#i', '', $c);
    $words = explode(' ', $c);
    foreach ($words as $i => $w) {
        $words[$i] = ($i > 0 && in_array(strtolower($w), ['and', 'or', 'of'])) ? strtolower($w)
            : (preg_match('#^[A-Z]{2,}$#', $w) ? $w : ucfirst(strtolower($w)));
    }
    return implode(' ', $words) . ' Functions';
}

/** First paragraphs after the "applies to" include, before the syntax or first heading */
function mssqlBody(string $md): array
{
    $md = preg_replace('#^---\n.*?\n---\n#s', '', $md);
    $md = preg_replace('#^\#[^\n]*\n#m', "\x02", $md, 1);
    $start = strpos($md, "\x02");
    $md = substr($md, $start === false ? 0 : $start + 1);
    $end = preg_match('#\n(\#\#|```)#', $md, $m, PREG_OFFSET_CAPTURE) ? $m[0][1] : strlen($md);
    $paragraphs = [];
    foreach (preg_split('#\n\s*\n#', substr($md, 0, $end)) as $p) {
        $p = markdownToText($p);
        if ($p !== '' && !preg_match('#^(Applies to|Transact-SQL syntax conventions)#i', $p)) {
            $paragraphs[] = $p;
        }
    }
    return array_slice($paragraphs, 0, 3);
}

function mssqlSection(string $md, string $headingRegex): string
{
    if (!preg_match('#\n\#\#\s+' . $headingRegex . '\s*\n(.*?)(?=\n\#\#|$)#is', $md, $m)) {
        return '';
    }
    foreach (preg_split('#\n\s*\n#', $m[1]) as $p) {
        $p = markdownToText($p);
        if ($p !== '') {
            return $p;
        }
    }
    return '';
}

function markdownToText(string $s): string
{
    $s = preg_replace('#\[!INCLUDE[^\]]*\]\([^)]*\)\]?#', '', $s);
    $s = preg_replace('#\[!NOTE\]|\[!TIP\]|\[!IMPORTANT\]|\[!WARNING\]|\[!CAUTION\]#', '', $s);
    $s = preg_replace('#:::[^\n]*:::#', '', $s);
    $s = preg_replace('#^\s*:::.*$#m', '', $s);
    $s = preg_replace('#!?\[([^\]]*)\]\([^)]*\)#', '$1', $s);
    $s = preg_replace('#<[^>]+>#', '', $s);
    $s = preg_replace('#^\s*>\s?#m', '', $s);
    $s = str_replace(['**', '`'], '', $s);
    $s = preg_replace('#(?<!\w)\*([^*\n]+)\*(?!\w)#', '$1', $s);
    $s = preg_replace('#(?<!\w)_([^_\n]+)_(?!\w)#', '$1', $s);
    $s = html_entity_decode($s, ENT_QUOTES | ENT_HTML5, 'UTF-8');
    return trim(preg_replace('#\s+#', ' ', toAscii($s)));
}


// ---------------------------------------------------------------------------------------------------
// Firebird: online language references
// ---------------------------------------------------------------------------------------------------

function genFirebird(array $opt): array
{
    $versions = explode(',', $opt['versions'] ?? '3.0,4.0,5.0');
    $firstSeen = [];
    $entries = [];
    $newest = end($versions);
    foreach ($versions as $version) {
        $v = str_replace('.', '', $version);
        $url = "https://firebirdsql.org/file/documentation/html/en/refdocs/fblangref$v/firebird-$v-language-reference.html";
        $html = download($url) ?? throw new RuntimeException("Could not read $url");
        foreach (parseFirebirdReference($html, $url) as $e) {
            $firstSeen[strtoupper($e['name'])] ??= $version;
            if ($version === $newest) {
                $entries[] = $e;
            }
        }
    }
    foreach ($entries as &$e) {
        $first = $firstSeen[strtoupper($e['name'])];
        $e['since'] = $first === $versions[0] ? '' : $first;
    }
    return $entries;
}

function parseFirebirdReference(string $html, string $url): array
{
    $entries = [];
    // Split into h2/h3/h4 sections
    $parts = preg_split('#(<h[234] id="[^"]+">.*?</h[234]>)#s', $html, -1, PREG_SPLIT_DELIM_CAPTURE);
    $chapter = $category = '';
    for ($i = 1; $i < count($parts); $i += 2) {
        preg_match('#<h([234]) id="([^"]+)">(.*?)</h[234]>#s', $parts[$i], $mh);
        [$level, $id, $title] = [$mh[1], $mh[2], htmlToText($mh[3])];
        $title = preg_replace('#^[\d.]+\s*#', '', $title);
        $content = $parts[$i + 1] ?? '';
        if ($level === '2') {
            $chapter = $title;
            continue;
        }
        if (!preg_match('#Functions#i', $chapter)) {
            continue;
        }
        // Function headings: "LPAD()", "CHAR_LENGTH(), CHARACTER_LENGTH()". Mostly h4 below a category h3,
        // but some on h3 level in older references.
        $isFunction = preg_match_all('#\b([A-Z][A-Z0-9_$]*)\(\)#', $title, $mn);
        if (!$isFunction && $level === '4') {
            // Older references: "<code>LAG</code>" without parentheses
            $isFunction = preg_match_all('#<code>([A-Z][A-Z0-9_$]*)</code>#', $mh[3], $mn);
        }
        if ($level === '3' && !$isFunction) {
            $category = $title;
            continue;
        }
        if (!$isFunction) {
            continue;
        }
        if ($level === '3') {
            $category = $chapter;
        }
        $content = preg_replace('#<div class="sect4">.*$#s', '', $content);
        $syntax = preg_match('#<div class="title">Syntax</div>\s*<div class="content">\s*<pre[^>]*>(.*?)</pre>#s', $content, $ms)
            ? html_entity_decode(strip_tags($ms[1]), ENT_QUOTES | ENT_HTML5, 'UTF-8') : '';
        $syntax = preg_replace('#\s+#', ' ', $syntax);
        $resultType = preg_match('#<div class="title">Result type</div>\s*<p>(.*?)</p>#s', $content, $mr) ? htmlToText($mr[1]) : '';
        // Prose: paragraphs and list items outside tables and titled blocks
        $prose = preg_replace('#<table.*?</table>#s', '', $content);
        // Titled paragraphs: "Result type", "See also"
        $prose = preg_replace('#<div class="paragraph">\s*<div class="title">.*?</div>\s*<p>.*?</p>\s*</div>#s', '', $prose);
        $prose = preg_replace('#<div class="listingblock">.*?</pre>\s*</div>\s*</div>#s', '', $prose);
        preg_match_all('#<p>(.*?)</p>#s', $prose, $mp);
        $paragraphs = [];
        if ($resultType !== '') {
            $paragraphs[] = 'Result type: ' . $resultType;
        }
        foreach ($mp[1] as $p) {
            $text = htmlToText($p);
            if ($text !== '' && count($paragraphs) < 5) {
                $paragraphs[] = $text;
            }
        }
        $description = buildDescription($paragraphs, "$url#$id");
        $headingDecls = [];
        foreach (array_unique($mn[1]) as $name) {
            $found = false;
            $offset = 0;
            // "CEIL[ING] (number)"
            while (preg_match('#\b' . preg_quote($name, '#') . '(\[\w+\])?\s*\(#', $syntax, $mf, PREG_OFFSET_CAPTURE, $offset)) {
                $parenPos = $mf[0][1] + strlen($mf[0][0]) - 1;
                $decl = balancedParens($syntax, $parenPos);
                if ($decl === null) {
                    break;
                }
                $entries[] = entry($name, $decl, $category, $description);
                $headingDecls[] = $decl;
                $offset = $parenPos + strlen($decl) + 2;
                $found = true;
            }
            if (!$found) {
                // Synonym in the same heading, e.g. CEILING with syntax "CEIL[ING] (number)"
                foreach ($headingDecls ?: [''] as $decl) {
                    $entries[] = entry($name, $decl, $category, $description);
                }
            }
        }
    }
    return $entries;
}


// ---------------------------------------------------------------------------------------------------
// Oracle: online SQL Language Reference
// ---------------------------------------------------------------------------------------------------

function genOracle(array $opt): array
{
    $versions = explode(',', $opt['versions'] ?? '19,21,26');
    $newest = end($versions);
    // Documentation release => version reported by the server. 26ai reports 23.x, like 23ai.
    $serverVersion = fn(string $v): string => $v === '26' ? '23' : $v;
    $firstSeen = [];
    $pages = [];
    foreach ($versions as $version) {
        foreach (oracleFunctionPages($version) as $page => $info) {
            $firstSeen[$page] ??= $version;
            if ($version === $newest) {
                $pages[$page] = $info;
            }
        }
    }
    $entries = [];
    $skipped = [];
    foreach ($pages as $page => $info) {
        $url = oracleDocsUrl($newest) . $info['href'];
        $html = download($url);
        if ($html === null) {
            continue;
        }
        $syntax = oracleSyntax($html, $newest, $info['label']);
        $since = $firstSeen[$page] === $versions[0] ? '' : $serverVersion($firstSeen[$page]);
        $description = buildDescription(oraclePurpose($html), $url);
        if ($syntax === null) {
            // Placeholder: completes a supplement entry for this name, dropped otherwise
            $skipped[] = $info['label'];
            $e = entry(preg_replace('#[^A-Z0-9_$\#].*$#', '', $info['label']), '', $info['category'], $description, $since);
            $e['placeholder'] = true;
            $entries[] = $e;
            continue;
        }
        foreach ($syntax['items'] as [$name, $declaration]) {
            $entries[] = entry($name, $declaration, $info['category'], $description, $since);
        }
        // Group page "STATS_T_TEST_*": placeholder "STATS_T_TEST_" for supplement entries of names not parsed
        if (preg_match('#^(\w+_)(\s|\*)#', $info['label'], $mg)) {
            $e = entry($mg[1], '', $info['category'], $description, $since);
            $e['placeholder'] = true;
            $entries[] = $e;
        }
    }
    if ($skipped) {
        fwrite(STDERR, 'No syntax with parentheses, only listed when in supplements: ' . implode(', ', $skipped) . "\n");
    }
    return $entries;
}

function oracleDocsUrl(string $version): string
{
    return "https://docs.oracle.com/en/database/oracle/oracle-database/$version/sqlrf/";
}

/**
 * Function pages listed on the overview pages of one documentation version, linked from Functions.html:
 * Single-Row-Functions.html with one category per h2 heading, Aggregate-Functions.html etc. with its title.
 * @return array lowercase page file name => [href, label (link text), category]. First category wins.
 */
function oracleFunctionPages(string $version): array
{
    $base = oracleDocsUrl($version);
    $index = download($base . 'Functions.html') ?? throw new RuntimeException("Could not read {$base}Functions.html");
    preg_match_all('#<a\s+href="([\w.-]+-Functions\.html)\##', $index, $m);
    $result = [];
    foreach (array_unique($m[1]) as $overview) {
        if (str_starts_with($overview, 'About-')) {
            continue;
        }
        $html = download($base . $overview);
        if ($html === null) {
            continue;
        }
        $html = preg_replace('#^.*?<article[^>]*>|</article>.*$#s', '', $html);
        // Page title is h1 in newer, h2 in older documentation. Categories one level below.
        $perSubHeading = $overview === 'Single-Row-Functions.html';
        $parts = preg_split('#(<h[1-4](?:\s[^>]*)?>.*?</h[1-4]>)#s', $html, -1, PREG_SPLIT_DELIM_CAPTURE);
        $category = '';
        $titleLevel = 0;
        foreach ($parts as $part) {
            if (preg_match('#^<h([1-4])(?:\s[^>]*)?>(.*?)</h[1-4]>$#s', $part, $mh)) {
                $level = (int)$mh[1];
                $titleLevel = $titleLevel ?: $level;
                if ($level === $titleLevel || ($perSubHeading && $level === $titleLevel + 1)) {
                    $category = htmlToText($mh[2]);
                }
                continue;
            }
            preg_match_all('#<li[^>]*>\s*(?:<p[^>]*>\s*)?<a\s+href="([\w.-]+\.html)(?:\#[^"]*)?"[^>]*>(.*?)</a>#s', $part, $ml, PREG_SET_ORDER);
            foreach ($ml as $l) {
                $label = htmlToText($l[2]);
                // Function names in upper case: "ABS", "ROUND (number)", "CORR_*", "REGR_ (Linear Regression) Functions"
                if (!preg_match('#^[A-Z][A-Z0-9_$\#]*(?![a-z])#', $label) || str_ends_with($l[1], '-Functions.html')) {
                    continue;
                }
                $result[strtolower($l[1])] ??= ['href' => $l[1], 'label' => $label, 'category' => $category];
            }
        }
    }
    if (!$result) {
        throw new RuntimeException("No functions found in documentation $version");
    }
    return $result;
}

/**
 * Names and declaration from the first text syntax diagram (img_text/*.html) of a function page which starts with
 * the function name, or alternative names in braces, followed by parentheses:
 * "{ SUBSTR | SUBSTRB | ... } (char, position [, substring_length ])" => SUBSTR, SUBSTRB, ...
 * @return array|null [names, items], see parseOracleSyntax()
 */
function oracleSyntax(string $html, string $version, string $label): ?array
{
    preg_match('#^[A-Z][A-Z0-9_$\#]*#', $label, $ml);
    $wanted = $ml[0];
    // Group pages like "CORR_*" or "REGR_ (Linear Regression) Functions" accept all names with that prefix
    $isGroup = (bool)preg_match('#^\w+(_\s|_?\*)#', $label);
    // Text diagrams: linked ones, and unlinked ones named like the diagram image (LISTAGG)
    preg_match_all('#href="(img_text/[\w.-]+\.html)"|<img src="[^"]*/images/([\w.-]+)\.gif"#', $html, $mi, PREG_SET_ORDER);
    $diagrams = [];
    foreach ($mi as $m) {
        $diagrams[] = $m[1] !== '' ? $m[1] : 'img_text/' . $m[2] . '.html';
    }
    foreach (array_slice(array_unique($diagrams), 0, 4) as $img) {
        $text = download(oracleDocsUrl($version) . $img, true);
        if ($text === null || !preg_match('#<pre[^>]*>(?:<code>)?(.*?)(?:</code>)?</pre>#s', $text, $mp)) {
            continue;
        }
        $syntax = parseOracleSyntax(html_entity_decode(strip_tags($mp[1]), ENT_QUOTES | ENT_HTML5, 'UTF-8'));
        if ($syntax === null) {
            continue;
        }
        // Exact name, or all names with the first word of the label as prefix:
        // CALENDAR => CALENDAR_YEAR ..., CALENDAR_START_END => CALENDAR_YEAR_START_DATE ...
        $matches = fn($n) => $isGroup ? str_starts_with($n, $wanted) : $n === $wanted;
        $prefix = preg_replace('#_.*$#', '', $wanted) . '_';
        $allPrefixed = !array_filter($syntax['names'], fn($n) => !str_starts_with($n, $prefix));
        if ($allPrefixed || array_filter($syntax['names'], $matches)) {
            return $syntax;
        }
    }
    return null;
}

/**
 * Parse a text syntax diagram:
 *   NAME (args)                          => [NAME], args
 *   { NAME1 | NAME2 } (args)             => [NAME1, NAME2], args
 *   NAME [DISTINCT | ALL] (expr)         => [NAME], [DISTINCT | ALL] expr
 *   NAME { (args) [...] | (args2) } ...  => [NAME], args
 *   { A (x) ) | ( B (x, y) ) }           => A, x and B, x, y
 *   name = ( "NAME" "(" "arg" ")" );     => EBNF style of newer pages, quotes removed first
 * @return array|null [names, items: [[name, declaration], ...]], null for functions without parentheses
 */
function parseOracleSyntax(string $s): ?array
{
    $s = trim(preg_replace('#\s+#', ' ', toAscii($s)));
    if (preg_match('#^\w+ = \((.*)\)\s*;?$#', $s, $m)) {
        $s = trim(preg_replace('#"([^"]*)"#', '$1', $m[1]));
        $s = preg_replace(['#\(\s+#', '#\s+\)#', '#\s+,#'], ['(', ')', ','], $s);
    }
    $names = [];
    if (preg_match('#^\{\s*\(?\s*[A-Z][A-Z0-9_$\#]*\s*\(#', $s)) {
        // Alternatives with own arguments: "{ A ( x ) ) | ( B ( x, y ) ) | ... }". Only if all of them parse.
        $items = [];
        foreach (explode('|', trim($s, '{} ')) as $alt) {
            $alt = trim($alt);
            if (!preg_match('#^\(?\s*([A-Z][A-Z0-9_$\#]*)\s*(?=\()#', $alt, $ma)
                || ($decl = balancedParens($alt, strlen($ma[0]))) === null) {
                $items = [];
                break;
            }
            $items[] = [$ma[1], $decl];
        }
        if ($items) {
            return ['names' => array_column($items, 0), 'items' => $items];
        }
    }
    if (str_starts_with($s, '{') && ($end = strpos($s, '}')) !== false) {
        foreach (explode('|', substr($s, 1, $end - 1)) as $alt) {
            $names[] = trim($alt);
        }
        $rest = ltrim(substr($s, $end + 1));
    } elseif (preg_match('#^([A-Z][A-Z0-9_$\#]*)\b\s*#', $s, $mn)) {
        $names[] = $mn[1];
        $rest = substr($s, strlen($mn[0]));
    } else {
        return null;
    }
    $names = array_values(array_filter($names, fn($n) => preg_match('#^[A-Z][A-Z0-9_$\#]*$#', $n)));
    if (!$names) {
        return null;
    }
    // Modifiers before the parentheses, kept in the declaration
    $prefix = '';
    while (preg_match('#^(\[[^\[\]]*\])\s*#', $rest, $mm)) {
        $prefix .= $mm[1] . ' ';
        $rest = substr($rest, strlen($mm[0]));
    }
    if (str_starts_with($rest, '{')) {
        // Alternative argument lists: take the first one
        $rest = ltrim(substr($rest, 1));
    }
    if (!str_starts_with($rest, '(') || ($decl = balancedParens($rest, 0)) === null) {
        return null;
    }
    return ['names' => $names, 'items' => array_map(fn($n) => [$n, $prefix . $decl], $names)];
}

/** Paragraphs of the "Purpose" section, list items prefixed with "- ", without notes and code */
function oraclePurpose(string $html): array
{
    // Some pages have an introduction between title and syntax
    $s = preg_match('#</h1>(.*?)<h2 #s', $html, $m) ? $m[1] : '';
    if (preg_match('#<h2 [^>]*>Purpose</h2>(.*?)(?:<h2 |</article>)#s', $html, $m)) {
        $s .= $m[1];
    } elseif (preg_match('#<h2 [^>]*>Syntax</h2>(.*?)(?:<h2 [^>]*>Examples?</h2>|</article>)#s', $html, $m)) {
        // No "Purpose" heading: text below the syntax diagrams
        $s .= preg_replace('#<h2 .*?</h2>|<p>\s*<img .*?</p>|<p>\s*<a href="img_text/.*?</p>#s', '', $m[1]);
    }
    $s = preg_replace('#<div class="infobox.*?</div>#s', '', $s);
    $s = preg_replace('#<p class="infobox.*?</p>#s', '', $s);
    $s = preg_replace('#<pre.*?</pre>#s', '', $s);
    $s = preg_replace('#<li>\s*<p>#', '<p>- ', $s);
    preg_match_all('#<p>(.*?)</p>#s', $s, $mp);
    $paragraphs = [];
    foreach ($mp[1] as $p) {
        $text = htmlToText($p);
        // Skip references to syntax clauses: "(listagg_overflow_clause::=, order_by_clause::=)"
        if ($text !== '' && !str_contains($text, '::=') && count($paragraphs) < 8) {
            $paragraphs[] = $text;
        }
    }
    return $paragraphs;
}
