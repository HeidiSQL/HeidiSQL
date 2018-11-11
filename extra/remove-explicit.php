<?php

if($argc < 2) {
    die("Usage:\n  ".$argv[0]." path\\to\dfmfiles\n");
}

$path = $argv[1];
if(!is_dir($path)) {
    die("Error: \"".$path."\" is not a valid directory.\n");
}

$files = glob($path.'\\*.dfm');
#var_dump($files);
$replaceCountAll = $touchedFileCount = 0;
foreach($files as $file) {
    $fileTime = filemtime($file);
    $dfm = file_get_contents($file);
    $replaceCount = 0;
    $dfm = preg_replace('# *Explicit\w+\s+\=\s+\d+\r\n#i', '\\1', $dfm, -1, $replaceCount);
    if($replaceCount > 0) {
        $replaceCountAll += $replaceCount;
        $touchedFileCount++;
        file_put_contents($file, $dfm);
        touch($file, $fileTime);
    }
}

echo $replaceCountAll." lines from ".$touchedFileCount." files removed\n";