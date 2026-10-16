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

$functionTexts = [];
$nl = "\r\n";
$hSyntax = "Syntax\n------";
$hDesc = "Description\n-----------";

// Syntax
// ------ 
// ADDDATE(date,INTERVAL expr unit), ADDDATE(expr,days)
// 
// Description
// ----------- 

while($row = mysqli_fetch_object($query)) {
	$name = $row->name;
	$desc = str_replace("\r\n", "\n", $row->description);

	$pSyntax = strpos($desc, $hSyntax);
	$pDesc = strpos($desc, $hDesc);
	
	if($pSyntax === false) {
		continue;
	}
	
	$syntax = substr($desc, $pSyntax, $pDesc-$pSyntax);
	$syntax = substr($syntax, strlen($hSyntax));
	$syntax = trim($syntax);
	$syntax = str_replace("\n", " ", $syntax);
	if(!preg_match('#^'.preg_quote($name).'\(#i', $syntax)) {
		continue;
	}
	$declarations = [];
	$parCount = 0;
	$decl = '';
	for($i=strpos($syntax, '('); $i<strlen($syntax); $i++) {
		if($syntax[$i] == '(') {
			$parCount++;
			$decl .= $syntax[$i];
		}
		else if($syntax[$i] == ')') {
			$parCount--;
			$decl .= $syntax[$i];
		}
		else if($parCount > 0) {
			$decl .= $syntax[$i];
		}
		
		if($parCount == 0 && !empty($decl)) {
			if(!in_array($decl, $declarations)) {
				$decl = str_replace("'", "''", $decl);
				$declarations[] = $decl;
			}
			$decl = '';
		}
	}
	
	if($pDesc !== false) {
		$descr = substr($desc, $pDesc);
		$descr = substr($descr, strlen($hDesc));
		$descr = preg_replace("#^\w+\n----(.*)#m", '', $descr);
        $descr = preg_replace('#\bURL\:\s+\S+#s', ' ', $descr);
		$descr = trim($descr);
		$descr = str_replace("'", "''", $descr);
		//$description = preg_replace('#(\s+)#', ' ', $description);
		//$description = wordwrap($description,72, " '".$nl."        +'");
		$descr = preg_split('#\r?\n#', $descr);
		$descr = implode("'+sLineBreak\r\n        +'", $descr);
	}
	else {
		$descr = '';
	}
	
	foreach($declarations as $declaration) {

		$functionTexts[] = sprintf("    (".$nl
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
			$descr
		);
		#break;
	}
}

// produce output
$counter = 0;
echo "  MySqlFunctions: Array [0..".(count($functionTexts)-1)."] of TMysqlFunction =" . $nl
  . "  (" . $nl;
foreach($functionTexts as $key=>$funcText)
{
    //echo '    // Function nr. '.++$counter.$nl;
    echo $funcText;
}
echo "  );" . $nl;