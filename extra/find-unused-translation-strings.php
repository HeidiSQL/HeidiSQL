<?php

/**
 * Find unused translation strings in po-file
 */

if(count($argv) < 3) {
    die('Usage: '.$argv[0].' <po-file> <pas-dfm-folder>');
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
    $sourceContents = file_get_contents($sourceFile);
    $sourceContents = preg_replace("#'\s*\+\s*'#", '', $sourceContents);
    foreach ($msgIds as $i=>$msgId) {
        // Double quote and backslash in translations are escaped with a backslash
        $msgId = str_replace('\"', '"', $msgId);
        $msgId = str_replace('\\\\', '\\', $msgId);
        // Delphi escapes a single quote with a second single quote
        $msgId = str_replace("'", "''", $msgId);
        if(str_contains($sourceContents, "'".$msgId."'")) {
            $stringsFound[$i]++;
        }
    }
}
#var_dump($stringsFound);
$unusedNum = 0;
$poContentsNew = $poContents;
foreach($msgIds as $i=>$msgId) {
    if($stringsFound[$i] == 0) {
        echo "Unused string #".(++$unusedNum).": \"".$msgId."\"\n";
        $poContentsNew = preg_replace("#(\r?\n\#[.:]\s+[^\n]*){0,10}\r?\nmsgid\s\"".preg_quote($msgId,'#')."\"\r?\nmsgstr\s\"".preg_quote($msgId, '#')."\"\r?\n#", '', $poContentsNew);
    }
}
#$poContentsNew = preg_replace("/(\r?\n)(\r?\n#[.:]\s+[^\n]*){1,10}\r?\n(\r?\n)/", '\\1\\3', $poContentsNew);
#if(preg_last_error() != PREG_NO_ERROR) {
#    throw new Exception(preg_last_error_msg());
#}

echo "\n";
echo "Used translation strings: ".(count($msgIds)-$unusedNum)."\n";

if($poContentsNew != $poContents) {
    $poFileNew = $poFile.'-without-unused';
    $bytesWritten = file_put_contents($poFileNew, $poContentsNew);
    echo "New file written with ".(strlen($poContents)-strlen($poContentsNew))." removed bytes: ".$poFileNew."\n";
}