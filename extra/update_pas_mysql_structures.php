<?php
/**
 * This is a helper file for generating the MySQLFunctions Array
 * in dbstructures.pas
 * Functions are fetched by using the HELP commands
 */


$mysqli = mysqli_connect('localhost', 'root');
$query = mysqli_query($mysqli, "SELECT t.name, t.description, c.name AS categ
	FROM mysql.help_topic t, mysql.help_category c
	WHERE
		t.help_category_id = c.help_category_id
	ORDER BY c.name, t.name");
if(mysqli_errno($mysqli)) {
    die (mysqli_error($mysqli));
}

$fstruc = [];
$nl = "\r\n";

while($row = mysqli_fetch_object($query)) {
    $isFunc = preg_match('#^(Syntax\:\s*)?'.preg_quote($row->name).'(\([^\)]*\))([^\n]*\n)?(.*)$#is', $row->description, $matches);
    if($isFunc) {
        //var_dump($matches);
        $declaration = $matches[2];
        $declaration = str_replace("'", "''", $declaration);

        $description = $matches[4];
        $description = preg_replace('#\bURL\:\s+\S+#s', ' ', $description);
        $description = str_replace("'", "''", $description );
        //$description = preg_replace('#(\s+)#', ' ', $description);
        //$description = wordwrap($description,72, " '".$nl."        +'");
        $description = trim($description);
        $description = preg_split('#\r?\n#', $description);
        $description = implode("'+sLineBreak\r\n        +'", $description);
        $fstruc[$row->name] = sprintf("    (".$nl
            ."      Name:         '%s';".$nl
            ."      Declaration:  '%s';".$nl
            ."      Category:     '%s';".$nl
            ."      Version:      %s;".$nl
            ."      Description:  '%s'".$nl
            ."    ),".$nl.$nl,
            $row->name,
            $declaration,
            $row->categ,
            'SQL_VERSION_ANSI',
            $description
        );
        #break;
    }
}
#die();
// Sort alphabetically by function name
asort($fstruc);

// produce output
$counter = 0;
foreach( $fstruc as $func )
{
    echo '    // Function nr. '.++$counter.$nl;
    echo $func;
}
