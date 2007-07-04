<?php
/**
 * This is a helper file for generating the MySQLFunctions Array
 * in /source/mysql.pas
 * Functions are fetched by using the HELP commands 
 */  

// Specify your own host, user, pass here.
// Do NOT commit passwords into SVN!!
mysql_connect( 'localhost', 'root' );

$nl = "\r\n";
$fnames = array();
$fstruc = array();

$q = mysql_query('HELP "functions"');
while( $row = mysql_fetch_object($q) )
{
	if( $row->is_it_category == 'Y' )
	{
		getfunctions($row->name);
	}
}

function getfunctions($cat)
{
	global $nl, $fstruc, $fnames;
	$q = mysql_query('HELP "'.$cat.'"');
	while( $row = mysql_fetch_object($q) )
	{
		if( $row->is_it_category == 'Y' )
		{
			getfunctions( $row->name );
		}
		else
		{
			$sql = "HELP '".$row->name."'";
			$qdetails = mysql_query($sql);
			$rowdetails = mysql_fetch_object($qdetails);

			if( preg_match('#(\S+)#', $rowdetails->name, $m1) )
			{
				$name = $m1[1];
			}
			else
			{
				$name = $row->name;
			}

			if( in_array($name,$fnames) )
			{
				continue;
			}
			$fnames[] = $name; 

			$declaration = '';
			$desc_cut = substr($rowdetails->description, 0, strpos($rowdetails->description,"\n\n")); 
			$df = preg_match('#Syntax:[^\(]*(\([^\)]*\))#Us', $desc_cut, $m2);
			if( $df )
			{
				$declaration = $m2[1];
				$declaration = str_replace("'", "''", $declaration );
			}

			$description = '';
			$df = preg_match('#Syntax:.*\n\n(.+)$#Uis', $rowdetails->description, $m3);
			if( $df )
			{
				$description = trim($m3[1]);
				$description = preg_replace('#(\s+)#', ' ', $description );
				$description = str_replace(' o ', ' ', $description);
				$description = str_replace("'", "''", $description );
				$description = wordwrap($description,70, " '".$nl."        +'" );
			}

			$fstruc[$name] .= sprintf("    (".$nl
				."      Name:         '%s';".$nl
				."      Declaration:  '%s';".$nl
				."      Category:     '%s';".$nl
				."      Description:  '%s'".$nl
				."    ),".$nl.$nl,
				$name,
				$declaration,
				$cat,
				$description
				);
		}
	}
}

// Sort alphabetically by function name
asort($fstruc);

// produce output
$counter = 0;
foreach( $fstruc as $func )
{
	print '    // Function nr. '.++$counter.$nl; 
	print $func;
}

?>
