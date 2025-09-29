<?php

/**
 * Script for building HeidiSQL main executable, in 64 bit
 * Syntax:
 *   php build.php
 *
 * The compiled binaries will include
 * - version number with Git revision, e.g. 12.11.0.7000
 * - compiled resource files
 * - translations downloaded from Transifex
 * - madExcept exception handler
 *
 * After successful compiling, you may load the .iss file in InnoSetup to create an installer
 *
 * @todo convert code from PHP to PowerShell
 */

$start_dir = getcwd();

const APPNAME = 'HeidiSQL';
const BIN_NAME = 'heidisql'; // file name of main executable
const DS = DIRECTORY_SEPARATOR;
const BASE_DIR = __DIR__ . DS;
const PACKAGE_DIR = 'Delphi12.3';
const PACKAGE_DIRS_COMPONENTS = [PACKAGE_DIR, 'RAD Studio 10.4+'];
const STUDIO_DIR = 'C:\\Program Files (x86)\\Embarcadero\\Studio\\23.0\\';
const COMPILER_DIR = STUDIO_DIR . 'bin\\';
const LIB_DIR = STUDIO_DIR . 'lib\\';
const MAD_DIR = 'C:\\Program Files (x86)\\madCollection\\';


function dumpMessage(string $text = '', bool $blankLineAbove = false): void
{
    if ($blankLineAbove)
        dumpMessage();
    $prefix = date('H:i:s') . ' ';
    echo $prefix . $text . PHP_EOL;
}

function compilerCommand(int $bit, string $outputNameExtension): string
{
    $params = [
        //'-$O-', // disable optimization
        '-$W+', // Generate stack frames
        '--no-config', // do not load default dcc64.cfg file
        //'-M', // Modifizierte Units erzeugen
        '-Q', // Quiet compile, workaround for avoiding error D21153 - see http://qc.borland.com/wc/qcmain.aspx?d=44731
        '-TX.'.$outputNameExtension, // Erweiterung des Ausgabenamens
        '-I"'.BASE_DIR.'source"', // include directories
        '-LE"..\..\build\Win'.$bit.'"', // package .bpl output directory
        '-LN"..\..\build\Win'.$bit.'"', // package .dcp output directory
        '-N0"..\..\build\Win'.$bit.'"', // unit .obj output directory
        '-NS"' // "Namespace search path" (or "Unit scope names" in IDE), used by SynEdit and probably VirtualTrees
            .'Vcl;'
            .'System;'
            .'Winapi;'
            .'System.Win;'
            .'Data;'
            .'"',
        '-R"' // Resource directories
            .BASE_DIR.'components\synedit\Source;'
            .BASE_DIR.'components\virtualtreeview\Source'
            .'"',
        '-U"' // Unit directories
            .LIB_DIR.'win'.$bit.'\release;'
            .BASE_DIR.'components\virtualtreeview\Source;'
            .BASE_DIR.'components\synedit\Source;'
            .BASE_DIR.'source\detours\Source;'
            .BASE_DIR.'source\vcl-styles-utils;'
            .BASE_DIR.'source\sizegrip;'
            .MAD_DIR.'madExcept\BDS23\win'.$bit.';'
            .MAD_DIR.'madDisAsm\BDS23\win'.$bit.';'
            .MAD_DIR.'madBasic\BDS23\win'.$bit.';'
            .'"',
        //'-K00400000', // Image-Basisadresse
        '-DmadExcept;DEBUG', // define conditionals
        '-GD', // detailed map file
        '--high-entropy-va:off', // ASLR, error on startup, no main form, then crash
        //'--dynamic-base:off', // ASLR since Vista, works here
        '-W-SYMBOL_PLATFORM', // disable output of some warning messages
        '-W-UNIT_PLATFORM',
        '-W-DUPLICATE_CTOR_DTOR',
        '-B', // build all units
        ];
    return '"'.COMPILER_DIR . 'dcc'.$bit.'.exe" ' . implode(' ', $params);
}


/**
 * Call the compiler for a component package
 */
function compileComponent($componentDir, $packageFile, $bit): bool
{
    dumpMessage('Compile component '.$componentDir.', package '.$packageFile.'...', true);
    foreach(PACKAGE_DIRS_COMPONENTS as $dir) {
        $fullDir = BASE_DIR . 'components\\' . $componentDir . '\\packages\\' . $dir;
        if(file_exists($fullDir)) {
            chdir($fullDir);
            return execCommand(compilerCommand($bit, 'bpl').' '.$packageFile);
        }
    }
    throw new \Exception('Could not find package folder for '.$componentDir.' component.');
}


/**
 * Run a command line, displays its output, then returns true/false on success or error,
 * or an array of strings if returnOutput is true
 */
function execCommand(string $command, bool $returnOutput=false): array|bool
{
    $output = [];
    $resultCode = 0;
    dumpMessage(getcwd().'>> '.$command);
    exec($command.' 2>&1', $output, $resultCode);
    if(!$returnOutput) {
        foreach ($output as $oline) {
            dumpMessage('# ' . ($resultCode ? 'Error: ' : '') . $oline);
        }
    }
    $success = $resultCode == 0;
    if(!$success) {
        dumpMessage('Last command failed, terminating.');
        exit(1);
    }
    return $returnOutput ? $output : $success;
}


/**
 * Return file names from given directory, recursively
 * @param string $path file path
 * @param string $filepattern file pattern, e.g. "*.xml"
 * @return array files
 */
function globRecursive(string $path, string $filepattern): array
{
    static $filecount=0;
    $path = rtrim($path, '/\\');

    // Find files in path
    $files = glob($path . DS . $filepattern, GLOB_BRACE);
    $filecount += count($files);

    // Find subdirectories in path, and do recursion
    $dirs = glob($path . DS . '*', GLOB_ONLYDIR);
    foreach($dirs as $d)
    {
        $files = array_merge($files, globRecursive($d, $filepattern));
    }

    return $files;
}


chdir(BASE_DIR);
dumpMessage('Detect Git revision...');
$gitCommits = execCommand('git log --pretty=oneline', true);
if(empty($gitCommits)) {
    die('No commits found.');
}
$lastCommitRevision = count($gitCommits) + 671; // The number of earlier Subversion commits which I could not migrate to Git
$lastCommitHash = substr($gitCommits[0], 0, strpos($gitCommits[0], ' '));

// start the build process
dumpMessage('Compile commit '.$lastCommitHash.' (revision '.$lastCommitRevision.')', true);
chdir(BASE_DIR);

dumpMessage('Remove unversioned files...');
execCommand('git clean -dfx');

dumpMessage('Download fresh translation files ...', true);
execCommand('extra\\internationalization\\tx.exe pull -a');

dumpMessage('Compile .po translation files...');
$po_files = globRecursive('out\\locale\\', '*.po');
foreach($po_files as $po_file)
{
    $mo_file = preg_replace('#\.po$#', '.mo', $po_file);
    execCommand('"extra\\internationalization\\msgfmt.exe" -o '.$mo_file.' '.$po_file);
}

$compileBits = ['64'];

foreach($compileBits as $bit)
{
    dumpMessage('********* Compile '.$bit.' bit executable', true);
    chdir(BASE_DIR);

    compileComponent('synedit', 'SynEdit_R.dpk', $bit);

    compileComponent('virtualtreeview', 'VirtualTreesR.dpk', $bit);


    chdir(BASE_DIR);
    $versionFile = realpath('res\\version.rc');
    dumpMessage('Revert version resource file...', true);
    execCommand('git checkout '.$versionFile);
    dumpMessage('Modify version resource file...');
    $versionOriginal = file_get_contents($versionFile);
    $versionRevision = preg_replace('#(FILEVERSION\s+\d+,\d+,\d+,)(\d+)(\b)#i', '${1}'.$lastCommitRevision.'$3', $versionOriginal);
    $versionRevision = str_replace('%APPNAME%', APPNAME, $versionRevision);
    preg_match('#FILEVERSION\s+(\d+),(\d+),(\d+),(\d+)\b#i', $versionRevision, $matches);
    $shortVersion = $matches[1].'.'.$matches[2].'.'.$matches[3].'.'.$matches[4];
    $fullVersion = $shortVersion.' '.$bit.' Bit';
    $versionRevision = str_replace('%APPVER%', $fullVersion, $versionRevision);
    file_put_contents($versionFile, $versionRevision);

    dumpMessage('Compile resource files...', true);
    execCommand('"'.COMPILER_DIR . 'brcc32.exe" '.$versionFile);
    execCommand('"'.COMPILER_DIR . 'cgrc.exe" res\\icon.rc');
    execCommand('"'.COMPILER_DIR . 'brcc32.exe" res\\icon-question.rc');
    execCommand('"'.COMPILER_DIR . 'brcc32.exe" res\\manifest.rc');
    execCommand('"'.COMPILER_DIR . 'brcc32.exe" res\\updater.rc');
    execCommand('"'.COMPILER_DIR . 'cgrc.exe" res\\styles.rc');
    execCommand('"'.COMPILER_DIR . 'brcc32.exe" source\\vcl-styles-utils\\AwesomeFont.rc');
    execCommand('"'.COMPILER_DIR . 'brcc32.exe" source\\vcl-styles-utils\\AwesomeFont_zip.rc');

    dumpMessage('Compile main project...', true);
    chdir(BASE_DIR.'packages\\'.PACKAGE_DIR);
    execCommand(compilerCommand($bit, 'exe').' -E"'.BASE_DIR.'out" heidisql.dpr');

    dumpMessage('Patch executable with .mo files...', true);
    // Must be done before madExcept writes a new crc header, otherwise it will complain about a corrupt .exe
    // See http://tech.dir.groups.yahoo.com/group/dxgettext/message/3623
    chdir(BASE_DIR);
    execCommand('extra\\internationalization\\assemble.exe out\\'.BIN_NAME.'.exe --dxgettext');

    dumpMessage('Patch executable with exception handler...', true);
    chdir(BASE_DIR.'packages\\'.PACKAGE_DIR);
    execCommand('"'.MAD_DIR.'madExcept\\Tools\\madExceptPatch.exe" "'.BASE_DIR.'out\\'.BIN_NAME.'.exe" heidisql.mes');

    chdir(BASE_DIR);
    $renameTo = sprintf('out\\%s%d.exe', BIN_NAME, $bit);
    dumpMessage('Rename to '.$renameTo.'...', true);
    rename('out\\'.BIN_NAME.'.exe', $renameTo);

}


chdir($start_dir);

