<?php
/**
 * Checks for the generated functions-*.ini files. See README.md.
 *
 * Usage:
 *   php check-functions.php diff [--base=HEAD] [files...]
 *       Compares function names and since/until per function between a git revision and the working copy.
 *       Default files: all functions-*.ini in this directory.
 *
 *   php check-functions.php mysql --port=3309 [--host=127.0.0.1] [--user=root] [--pass=] [--file=functions-mysql.ini]
 *       Applies HeidiSQL's since/until filter for the version of a running MySQL or MariaDB server and compares
 *       the result with the server's help topics. Functions in the help but hidden by the filter point to a
 *       wrong since/until. Also works with --file=functions-mariadb.ini against a MariaDB server.
 *
 *   php check-functions.php postgresql --psql=<path to psql> [--host=127.0.0.1] [--port=5432] [--user=postgres]
 *       [--pass=] [--file=functions-postgresql.ini]
 *       Same, against pg_proc of a running PostgreSQL server. SQL keywords like COALESCE are not in pg_proc
 *       and therefore reported as "not on server", which is fine.
 */

declare(strict_types=1);

$args = ['options' => []];
foreach (array_slice($argv, 1) as $arg) {
    if (preg_match('#^--([\w-]+)=(.*)$#s', $arg, $m)) {
        $args['options'][$m[1]] = $m[2];
    } else {
        $args[] = $arg;
    }
}
$opt = $args['options'];
$command = $args[0] ?? '';

switch ($command) {
    case 'diff':
        $files = array_slice(array_filter($args, 'is_int', ARRAY_FILTER_USE_KEY), 1);
        if (!$files) {
            $files = array_map('basename', glob(__DIR__ . '/functions-*.ini'));
        }
        foreach ($files as $file) {
            checkDiff($file, $opt['base'] ?? 'HEAD');
        }
        break;
    case 'mysql':
        checkMysql($opt);
        break;
    case 'postgresql':
        checkPostgresql($opt);
        break;
    default:
        fwrite(STDERR, "Unknown command \"$command\". See header of " . basename(__FILE__) . " for usage.\n");
        exit(1);
}


/** @return array name (upper case) => [since, until] of each section; overloads: the widest range */
function parseIni(string $text): array
{
    $result = [];
    $cur = null;
    foreach (preg_split('#\r?\n#', $text) as $line) {
        if (preg_match('#^\[(.+)\]$#', $line, $m)) {
            if ($cur) {
                addEntry($result, $cur);
            }
            $cur = ['name' => $m[1], 'since' => '', 'until' => ''];
        } elseif ($cur && preg_match('#^(name|since|until)=(.*)$#', $line, $m)) {
            $cur[$m[1]] = trim($m[2]);
        }
    }
    if ($cur) {
        addEntry($result, $cur);
    }
    return $result;
}

function addEntry(array &$result, array $e): void
{
    $u = strtoupper($e['name']);
    $result[$u] ??= [$e['since'], $e['until']];
}

/** Same as TSQLFunctionList.Create: "16", "3.44" or "18.1.2" => comparable number */
function versionToNumber(string $v): int
{
    if (!preg_match('#^(\d+)(\.(\d+))?(\.(\d+))?#', $v, $m)) {
        return 0;
    }
    return (int)$m[1] * 1000000 + (int)($m[3] ?? 0) * 1000 + (int)($m[5] ?? 0);
}

/** Names HeidiSQL shows for a server version */
function shownNames(array $entries, string $serverVersion): array
{
    $sv = versionToNumber($serverVersion);
    $shown = [];
    foreach ($entries as $name => [$since, $until]) {
        if ($sv > 0 && versionToNumber($since) > $sv) {
            continue;
        }
        if ($sv > 0 && $until !== '' && versionToNumber($until) <= $sv) {
            continue;
        }
        $shown[$name] = true;
    }
    return $shown;
}

function readIniFile(string $file): array
{
    $path = preg_match('#[/\\\\]#', $file) ? $file : __DIR__ . "/$file";
    $text = @file_get_contents($path);
    if ($text === false) {
        fwrite(STDERR, "Could not read $path\n");
        exit(1);
    }
    return parseIni($text);
}

function printList(string $title, array $names): void
{
    sort($names);
    echo $title . ' (' . count($names) . '): ' . implode(' ', $names) . "\n";
}

function checkDiff(string $file, string $base): void
{
    $old = shell_exec('git -C ' . escapeshellarg(__DIR__) . ' show ' . escapeshellarg("$base:./$file") . ' 2>' . (PHP_OS_FAMILY === 'Windows' ? 'NUL' : '/dev/null'));
    $old = parseIni((string)$old);
    $new = readIniFile($file);
    echo "== $file ==\n";
    printList('  removed', array_keys(array_diff_key($old, $new)));
    printList('  added', array_keys(array_diff_key($new, $old)));
    $changed = [];
    foreach (array_intersect_key($new, $old) as $name => $n) {
        if ($n !== $old[$name]) {
            $changed[] = sprintf('    %-32s since %-10s -> %-10s until %-10s -> %s', $name,
                $old[$name][0] ?: '.', $n[0] ?: '.', $old[$name][1] ?: '.', $n[1] ?: '.');
        }
    }
    sort($changed);
    echo '  since/until changed (' . count($changed) . "):\n" . ($changed ? implode("\n", $changed) . "\n" : '');
}

function checkMysql(array $opt): void
{
    mysqli_report(MYSQLI_REPORT_ERROR | MYSQLI_REPORT_STRICT);
    $db = mysqli_connect($opt['host'] ?? '127.0.0.1', $opt['user'] ?? 'root', $opt['pass'] ?? '', null, (int)($opt['port'] ?? 3306));
    $version = mysqli_fetch_row(mysqli_query($db, 'SELECT VERSION()'))[0];
    $res = mysqli_query($db, 'SELECT name FROM mysql.help_topic');
    $help = [];
    while ($row = mysqli_fetch_row($res)) {
        // "IF FUNCTION", "Median Window Function", "JSON_MERGE_PATCH()", like generate-functions.php
        if (preg_match('#^(\w+)(\(\))?(\s+(window|aggregate))?(\s+function)?$#i', $row[0], $m)) {
            $help[strtoupper($m[1])] = true;
        }
    }
    $entries = readIniFile($opt['file'] ?? 'functions-mysql.ini');
    $shown = shownNames($entries, $version);
    echo "Server $version, " . count($shown) . ' of ' . count($entries) . " functions shown\n";
    printList('Hidden by since/until, but in server help - check these', array_keys(array_diff_key(array_intersect_key($entries, $help), $shown)));
    printList('Shown, but not in server help (help gaps or statements, mostly fine)', array_keys(array_diff_key($shown, $help)));
}

function checkPostgresql(array $opt): void
{
    if (empty($opt['psql'])) {
        fwrite(STDERR, "Pass the path to psql with --psql=<path>\n");
        exit(1);
    }
    if (isset($opt['pass'])) {
        putenv('PGPASSWORD=' . $opt['pass']);
    }
    $sql = "SELECT current_setting('server_version'), string_agg(DISTINCT upper(proname), ' ') FROM pg_proc";
    $cmd = escapeshellarg($opt['psql']) . ' -h ' . escapeshellarg($opt['host'] ?? '127.0.0.1') . ' -p ' . escapeshellarg($opt['port'] ?? '5432')
        . ' -U ' . escapeshellarg($opt['user'] ?? 'postgres') . ' -At -F "|" -c ' . escapeshellarg($sql);
    $out = trim((string)shell_exec($cmd));
    if (!str_contains($out, '|')) {
        fwrite(STDERR, "psql failed: $out\n");
        exit(1);
    }
    [$version, $procs] = explode('|', $out, 2);
    $procs = array_fill_keys(explode(' ', $procs), true);
    $entries = readIniFile($opt['file'] ?? 'functions-postgresql.ini');
    $shown = shownNames($entries, $version);
    echo "Server $version, " . count($shown) . ' of ' . count($entries) . " functions shown\n";
    printList('Hidden by since/until, but in pg_proc - check these', array_keys(array_diff_key(array_intersect_key($entries, $procs), $shown)));
    printList('Shown, but not in pg_proc (SQL keywords like COALESCE are fine)', array_keys(array_diff_key($shown, $procs)));
}
