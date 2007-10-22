<?php
/**
 * This is a helper file for generating the MySQLFunctions Array
 * in /source/mysql.pas
 * Functions are fetched by using the HELP commands 
 */  

// Gather version information for functions
$ver_cont = file_get_contents('http://dev.mysql.com/doc/refman/5.2/en/func-op-summary-ref.html');
if( $ver_cont )
{
	// <code class="literal">ADDDATE()</code></a>(v4.1.1)
	$ver_cont = html_entity_decode($ver_cont);
	$matches = array();
	preg_match_all( '#\<a[^\>]+\>\<code class="literal"\>([^\(\s\<]+).*\<\/a\>\(v(.+)\)#', $ver_cont, $matches );
	$versions = array();
	for( $i=0; $i<count($matches[0]); $i++ )
	{
		$versionArr = explode( '.', $matches[2][$i] );
		// make int of array
		if( !isset($versionArr[1]) )
			$versionArr[1] = '0';
		if( !isset($versionArr[2]) )
			$versionArr[2] = '0';
		$version = sprintf('%d%02d%02d',
			$versionArr[0],
			$versionArr[1],
			$versionArr[2]
			);
		// functionname => version
		$versions[ $matches[1][$i] ] = $version;
	}
}

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
getfunctions('Functions and Modifiers for Use with GROUP BY');
getfunctions('Geographic Features');


function getfunctions( $cat, $rootcat='' )
{
	global $nl, $fstruc, $fnames, $versions;
	$q = mysql_query('HELP "'.$cat.'"');
	while( $row = mysql_fetch_object($q) )
	{
		if( $row->is_it_category == 'Y' )
		{
			if( empty($rootcat) )
			{
				$rootcat = $cat;
			}
			getfunctions( $row->name, $rootcat );
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
			$df = preg_match('#(Syntax:)?[^\(]*(\([^\)]*\))#Us', $desc_cut, $m2);
			if( $df )
			{
				$declaration = $m2[2];
				$declaration = str_replace("'", "''", $declaration );
			}

			$description = '';
			$df = preg_match('#(Syntax:)?.*\n\n(.+)$#Uis', $rowdetails->description, $m3);
			if( $df )
			{
				$description = trim($m3[2]);
				$description = preg_replace('#(\s+)#', ' ', $description );
				$description = str_replace(' o ', ' ', $description);
				$description = str_replace("'", "''", $description );
				$description = wordwrap($description,70, " '".$nl."        +'" );
			}

			$version = 'SQL_VERSION_ANSI';
			if( !empty($versions[$name]) )
			{
				$version = $versions[$name];
			}

			$fstruc[$name] .= sprintf("    (".$nl
				."      Name:         '%s';".$nl
				."      Declaration:  '%s';".$nl
				."      Category:     '%s';".$nl
				."      Version:      %s;".$nl
				."      Description:  '%s'".$nl
				."    ),".$nl.$nl,
				$name,
				$declaration,
				(!empty($rootcat) ? $rootcat : $cat),
				$version,
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
