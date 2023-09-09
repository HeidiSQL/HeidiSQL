<?php

/**
 * Find unused translation strings in po-file
 */

if(count($argv) < 3) {
    die('Usage: '.$argv[0].' <po-file> <pas-dfm-files>');
}

$poFile = $argv[1];
$sourcePath = $argv[2];

#var_dump($poFile);
#var_dump($sourcePath);

if(!file_exists($poFile)) {
    die('Error: PO file does not exist: '.$poFile);
}
if(!file_exists($sourcePath)) {
    die('Error: Source path does not exist: '.$sourcePath);
}

$poContents = file_get_contents($poFile);
if(!preg_match_all('#msgid\s"(.+)"#', $poContents, $matches)) {
    die('Error: No msgid items found in PO file');
}
$msgIds = $matches[1];
#var_dump($msgIds);

$sourceFiles = glob($sourcePath.'/*.{dfm,pas,inc}', GLOB_BRACE);
#var_dump($sourceFiles);

// Prefill helper array with 0's
$zeros = array_fill(0, count($msgIds), 0);
$stringsFound = array_combine(array_keys($msgIds), $zeros);

// Read files and count occurrences
foreach($sourceFiles as $sourceFile) {
    $fileContents = file_get_contents($sourceFile);
    foreach ($msgIds as $i=>$msgId) {
        $msgId = str_replace('\"', '"', $msgId);
        $msgId = str_replace('\\\\', '\\', $msgId);
        if(str_contains($fileContents, "'".$msgId."'")) {
            $stringsFound[$i]++;
        }
    }
}
#var_dump($stringsFound);
$unusedNum = 0;
foreach($msgIds as $i=>$msgId) {
    if($stringsFound[$i] == 0) {
        echo "Unused string #".(++$unusedNum).": \"".$msgId."\"\n";
    }
}