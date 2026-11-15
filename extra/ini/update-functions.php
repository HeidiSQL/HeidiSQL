<?php
/**
 * Regenerates all generated functions-*.ini files in one go. See README.md.
 *
 * Usage:
 *   php update-functions.php [--option=value ...]
 *
 * Options:
 *   --only=<targets>         Comma separated subset of: mariadb,mysql,postgresql,sqlite,mssql,firebird
 *   --cache=<dir>            Download cache, cleared first unless --keep-cache=1.
 *                            Default: system temp dir, like generate-functions.php
 *   --keep-cache=1           Keep cached downloads. New releases and doc changes are then not seen.
 *   --mariadb-docs=<dir>     Checkout of mariadb-corporation/mariadb-docs, default ../../../3rdparty/mariadb-docs
 *   --mssql-docs=<dir>       Checkout of MicrosoftDocs/sql-docs, default ../../../3rdparty/sql-docs
 *   --mysql-servers=<list>   Running MySQL servers for the help texts, ascending versions, port[:password],
 *                            e.g. 3300,3334:secret,3333,3308,3309. Without this option, mysql is skipped.
 *   --host=, --user=         For the MySQL servers, default 127.0.0.1 and root
 *   --pg-versions=<list>     PostgreSQL documentation versions, default from generate-functions.php
 *   --fb-versions=<list>     Firebird language reference versions, default from generate-functions.php
 *   --no-pull=1              Do not "git pull" the documentation checkouts
 *   --trunk-out=<dir>        Copy the resulting files to this directory, e.g. the out/ folder of the
 *                            Delphi checkout
 */

declare(strict_types=1);

$opt = [];
foreach (array_slice($argv, 1) as $arg) {
    if (!preg_match('#^--([\w-]+)=(.*)$#s', $arg, $m)) {
        fwrite(STDERR, "Unknown argument: $arg\n");
        exit(1);
    }
    $opt[$m[1]] = $m[2];
}

$dir = __DIR__;
$workspace = dirname($dir, 3);
$targets = isset($opt['only']) ? explode(',', $opt['only']) : ['mariadb', 'mysql', 'postgresql', 'sqlite', 'mssql', 'firebird'];
$cache = $opt['cache'] ?? (sys_get_temp_dir() . DIRECTORY_SEPARATOR . 'heidisql-funcgen');
$mariadbDocs = $opt['mariadb-docs'] ?? "$workspace/3rdparty/mariadb-docs";
$mssqlDocs = $opt['mssql-docs'] ?? "$workspace/3rdparty/sql-docs";

// 1. Clear download cache, so new release tags and documentation changes are seen
if (empty($opt['keep-cache']) && is_dir($cache)) {
    echo "Clearing cache $cache\n";
    foreach (new RecursiveIteratorIterator(new RecursiveDirectoryIterator($cache, FilesystemIterator::SKIP_DOTS),
                 RecursiveIteratorIterator::CHILD_FIRST) as $f) {
        $f->isDir() ? rmdir($f->getPathname()) : unlink($f->getPathname());
    }
}

// 2. Update documentation checkouts
if (empty($opt['no-pull'])) {
    foreach (['mariadb' => $mariadbDocs, 'mssql' => $mssqlDocs] as $target => $checkout) {
        if (in_array($target, $targets) && is_dir("$checkout/.git")) {
            run('git -C ' . escapeshellarg($checkout) . ' pull --ff-only');
        }
    }
}

// 3. Generate
$php = escapeshellarg(PHP_BINARY);
$gen = $php . ' ' . escapeshellarg("$dir/generate-functions.php");
$common = ' --cache=' . escapeshellarg($cache);
$written = [];
foreach ($targets as $target) {
    $args = '';
    switch ($target) {
        case 'mariadb':
            requireDir($mariadbDocs, 'mariadb-docs', 'https://github.com/mariadb-corporation/mariadb-docs');
            $args = ' --docs=' . escapeshellarg($mariadbDocs);
            break;
        case 'mysql':
            if (empty($opt['mysql-servers'])) {
                echo "Skipping mysql: no --mysql-servers given\n";
                continue 2;
            }
            $args = ' --servers=' . escapeshellarg($opt['mysql-servers']);
            foreach (['host', 'user'] as $k) {
                if (isset($opt[$k])) {
                    $args .= " --$k=" . escapeshellarg($opt[$k]);
                }
            }
            break;
        case 'postgresql':
            $args = isset($opt['pg-versions']) ? ' --versions=' . escapeshellarg($opt['pg-versions']) : '';
            break;
        case 'firebird':
            $args = isset($opt['fb-versions']) ? ' --versions=' . escapeshellarg($opt['fb-versions']) : '';
            break;
        case 'mssql':
            requireDir($mssqlDocs, 'sql-docs', 'https://github.com/MicrosoftDocs/sql-docs');
            $args = ' --docs=' . escapeshellarg($mssqlDocs);
            break;
        case 'sqlite':
            break;
        default:
            fwrite(STDERR, "Unknown target: $target\n");
            exit(1);
    }
    echo "\n== $target ==\n";
    run("$gen $target$common$args");
    $written[] = "functions-$target.ini";
}

// 4. Copy to the Delphi checkout
if (!empty($opt['trunk-out'])) {
    foreach ($written as $file) {
        copy("$dir/$file", rtrim($opt['trunk-out'], '/\\') . "/$file") or exit(1);
        echo "Copied $file to {$opt['trunk-out']}\n";
    }
}

echo "\nDone. Next: php check-functions.php diff, then review, build and test. See README.md.\n";


function run(string $cmd): void
{
    passthru($cmd, $code);
    if ($code !== 0) {
        fwrite(STDERR, "Failed with exit code $code: $cmd\n");
        exit($code);
    }
}

function requireDir(string $path, string $name, string $repo): void
{
    if (!is_dir($path)) {
        fwrite(STDERR, "Checkout of $name not found: $path\nSee README.md for creating a sparse checkout of $repo\n");
        exit(1);
    }
}
