<?php

const NL = "\r\n";

/**
 * @param array $iniEntries
 * @param bool $doWordWrap Set to false if descriptions already contain line breaks, like in MySQL
 * @return string
 */
function finalizeEntries(array $iniEntries, bool $doWordWrap): string
{
    static $replaceMap = [
        '“' => '"',
        '”' => '"',
        '—' => '-',
        '&nbsp;' => ' ',
    ];
    $numVersions = [];
    foreach($iniEntries as $iniEntry) {
        if(!isset($numVersions[$iniEntry['name']]))
            $numVersions[$iniEntry['name']] = 0;
        $numVersions[$iniEntry['name']]++;
    }
    // var_dump($numVersions);

    $sections = $finalEntries = [];
    foreach($iniEntries as $iniEntry) {
        if($numVersions[$iniEntry['name']] > 1) {
            for($i=1; $i<100; $i++) {
                $section = $iniEntry['name'] . $i;
                if(!in_array($section, $sections)) {
                    $sections[] = $section;
                    break;
                }
            }
        }
        else {
            $section = $iniEntry['name'];
            $sections[] = $section;
        }
        $entry = "[".$section."]".NL;
        if($section != $iniEntry['name']) {
            $entry .= "name=".$iniEntry['name'].NL;
        }
        $descr = $iniEntry['description'];
        $descr = strtr($descr, $replaceMap);
        // Limit description to 50 lines, if the rest is longer than 100 chars
        $numLinebreaks = 0;
        $lenDescr = strlen($descr);
        for($i=0; $i<strlen($descr); $i++) {
            if($descr[$i] == "\n") {
                $numLinebreaks++;
            }
            if($numLinebreaks == 50 && $lenDescr > $i + 100) {
                $descr = substr($descr, 0, $i+1) . ' ...';
                break;
            }
        }
        //die();
        if($doWordWrap) {
            $descr = wordwrap($descr);
        }
        $descr = str_replace(["\r\n", "\r", "\n"], '\n', $descr);

        $entry .= "declaration=".$iniEntry['declaration'].NL
            . "category=".$iniEntry['category'].NL
            . "description=".html_entity_decode($descr);
        $finalEntries[$section] = $entry;
    }
    ksort($finalEntries);
    //var_dump($finalEntries);
    return implode(NL, $finalEntries);
}


function gen_sqlite(): string
{
    $urls = [
        'Aggregate Functions'=>'https://www.sqlite.org/lang_aggfunc.html',
        'Scalar SQL Functions'=>'https://www.sqlite.org/lang_corefunc.html',
        'Window Functions' =>'https://www.sqlite.org/windowfunctions.html',
    ];

    $iniEntries = [];

    foreach($urls as $category=>$url) {
        //echo $url."\n";
        $contents = file_get_contents($url);
        /*
         * <dt><p><b>sum(<i>X</i>)<br />total(<i>X</i>)</b></dt><dd><p>
            some text</p>
            <p>some text</p>
            <p>some text
            </dd>
         */
        preg_match_all('#\<dt\>(.+)\</dt\>\s*\<dd\>(.+)\</dd\>#isU', $contents, $matches);
        //var_dump($matches);
        for($i=0; $i<count($matches[1]); $i++) {
            $defs = strip_tags(str_replace('<br />', "\n", $matches[1][$i]));
            //echo $defs."\n\n";
            $defs = explode("\n", $defs);
            foreach($defs as $def) {
                if(!preg_match('#^(\w+)\(([^\)]*)\)#', $def, $matchesDef)) {
                    continue;
                }
                //var_dump($matchesDef);
                $entry = ['name'=>strtoupper($matchesDef[1]), 'declaration'=>$matchesDef[2], 'category'=>$category];
                $descr = $matches[2][$i];
                $descr = preg_replace('#\</p\>\s*\<p\>#', '\\n', $descr);
                $descr = strip_tags($descr);
                $descr = preg_replace('#\s+#', ' ', $descr);
                $entry['description'] = trim($descr);
                //var_dump($entry);
                //break(2);
                $iniEntries[] = $entry;
            }
        }
        //break;
    }

    /*
     * non-parsable date functions:
    date(time-value, modifier, modifier, ...)
    time(time-value, modifier, modifier, ...)
    datetime(time-value, modifier, modifier, ...)
    julianday(time-value, modifier, modifier, ...)
    strftime(format, time-value, modifier, modifier, ...)
     */
    $iniEntries[] = [
        'name'=>'DATE',
        'declaration'=>'time-value, modifier, modifier, ...',
        'category'=>'Date And Time Functions',
        'description'=>'All five date and time functions take a time value as an argument. The time value is followed by zero or more modifiers. The strftime() function also takes a format string as its first argument.',
        ];
    $iniEntries[] = [
        'name'=>'TIME',
        'declaration'=>'time-value, modifier, modifier, ...',
        'category'=>'Date And Time Functions',
        'description'=>'All five date and time functions take a time value as an argument. The time value is followed by zero or more modifiers. The strftime() function also takes a format string as its first argument.',
    ];
    $iniEntries[] = [
        'name'=>'DATETIME',
        'declaration'=>'time-value, modifier, modifier, ...',
        'category'=>'Date And Time Functions',
        'description'=>'All five date and time functions take a time value as an argument. The time value is followed by zero or more modifiers. The strftime() function also takes a format string as its first argument.',
    ];
    $iniEntries[] = [
        'name'=>'JULIANDAY',
        'declaration'=>'time-value, modifier, modifier, ...',
        'category'=>'Date And Time Functions',
        'description'=>'All five date and time functions take a time value as an argument. The time value is followed by zero or more modifiers. The strftime() function also takes a format string as its first argument.',
    ];
    $iniEntries[] = [
        'name'=>'STRFTIME',
        'declaration'=>'format, time-value, modifier, modifier, ...',
        'category'=>'Date And Time Functions',
        'description'=>'All five date and time functions take a time value as an argument. The time value is followed by zero or more modifiers. The strftime() function also takes a format string as its first argument.',
    ];

    return finalizeEntries($iniEntries, true);
}


function gen_mysql(int $port)
{
    // Insert your custom password and port
    $mysqli = mysqli_connect('localhost', 'root', null, null, $port);
    $query = mysqli_query($mysqli, "SELECT t.name, t.description, c.name AS categ
        FROM mysql.help_topic t, mysql.help_category c
        WHERE
            t.help_category_id = c.help_category_id
		    AND c.name NOT LIKE 'Internal%'
            -- and t.name like 'CURRENT_TIMESTAMP'
        ORDER BY t.name");
    if(mysqli_errno($mysqli)) {
        die ('MySQL connection error: '.mysqli_error($mysqli));
    }
    $iniEntries = [];

    while($row = mysqli_fetch_object($query)) {
        $name = $row->name;
        // Exclude function names with spaces, or other non-word characters:
        if(!preg_match('#^\w+$#', $name)) {
            //echo "10\n";
            continue;
        }
        #echo $name."\n";
        $matchCount = preg_match(
            '#\b'.preg_quote($row->name).'\s?\[?\(([^\)]*)\)[^\r\n]*[\r\n](.*)$#is',
            $row->description,
            $matches);
        if(!$matchCount) {
            //echo "20\n";
            continue;
        }
        $declaration = trim($matches[1]);
        $declaration = preg_replace('#[\r\n]#', ' ', $declaration);

        $description = trim($matches[2]);
        if(preg_match('#Description\s+\-+[\r\n](.+)#is', $description, $matchesD)) {
            $description = trim($matchesD[1]);
        }
        //$description = preg_replace('#[\r\n]#', ' ', $description);
        #echo $row->name."\n".$matches[2]."\n".$matches[3]."\n\n";
        $iniEntries[] = [
            'name'=>$row->name,
            'declaration'=>$declaration,
            'category'=>$row->categ,
            'description'=>$description,
        ];

    }
    return finalizeEntries($iniEntries, false);
}


function gen_pg(): string
{
    /*
     * https://www.postgresql.org/docs/current/functions-string.html
     *
     *
             <td class="func_table_entry">
              <p class="func_signature"><a id="id-1.5.8.10.7.2.2.1.1.1.1" class="indexterm" name="id-1.5.8.10.7.2.2.1.1.1.1"></a> <code class="function">ascii</code> ( <code class="type">text</code> ) → <code class="returnvalue">integer</code></p>
              <p>Returns the numeric code of the first character of the argument. In <acronym class="acronym">UTF8</acronym> encoding, returns the Unicode code point of the character. In other multibyte encodings, the argument must be an <acronym class="acronym">ASCII</acronym> character.</p>
              <p><code class="literal">ascii('x')</code> → <code class="returnvalue">120</code></p>
            </td>
     */
    static $categoryUrls = [
        'Numeric/Math Functions' => 'https://www.postgresql.org/docs/current/functions-math.html',
        'String Functions' => 'https://www.postgresql.org/docs/current/functions-string.html',
        'Binary String Functions' => 'https://www.postgresql.org/docs/current/functions-binarystring.html',
        'Bit String Functions' => 'https://www.postgresql.org/docs/current/functions-bitstring.html',
        'Date/Time Functions' => 'https://www.postgresql.org/docs/current/functions-datetime.html',
        'Enum Support Functions' => 'https://www.postgresql.org/docs/current/functions-enum.html',
        'Geometric Functions' => 'https://www.postgresql.org/docs/current/functions-geometry.html',
        'Network Address Functions' => 'https://www.postgresql.org/docs/current/functions-net.html',
        'Text Search Functions' => 'https://www.postgresql.org/docs/current/functions-textsearch.html',
        'JSON Functions' => 'https://www.postgresql.org/docs/current/functions-json.html',
        'Sequence Manipulation Functions' => 'https://www.postgresql.org/docs/current/functions-sequence.html',
        'Array Functions' => 'https://www.postgresql.org/docs/current/functions-array.html',
        'Range Functions' => 'https://www.postgresql.org/docs/current/functions-range.html',
        'Aggregate Functions' => 'https://www.postgresql.org/docs/current/functions-aggregate.html',
        'Window Functions' => 'https://www.postgresql.org/docs/current/functions-window.html',
        'Merge Support Functions' => 'https://www.postgresql.org/docs/current/functions-merge-support.html',
        'Session Information Functions' => 'https://www.postgresql.org/docs/current/functions-info.html',
        'System Administration Functions' => 'https://www.postgresql.org/docs/current/functions-admin.html',
        'Trigger Functions' => 'https://www.postgresql.org/docs/current/functions-trigger.html',
        'Statistics Information Functions' => 'https://www.postgresql.org/docs/current/functions-statistics.html',
    ];

    $iniEntries = [];
    foreach($categoryUrls as $category => $url) {
        $doc = file_get_contents($url);
        if(empty($doc)) {
            throw new RuntimeException("Could not read $url");
        }
        $numMatches = preg_match_all('#<p class="func_signature"><a\s*[^>]*></a>\s*<code class="function">(\w+)</code>\s*\(([^)]*)\).*</p>\s*<p>(.+)</p>#', $doc, $matches);
        if($numMatches === false) {
            throw new RuntimeException("Regexp error: ".preg_last_error());
        }
        #var_dump($matches);
        foreach($matches[1] as $i=>$name) {
            $iniEntries[] = [
                'name' => strtoupper($name),
                'declaration' => trim(strip_tags($matches[2][$i])),
                'category' => $category,
                'description' => trim(strip_tags($matches[3][$i])),
            ];
        }
        #break;
    }
    return finalizeEntries($iniEntries, true);
}

// SQLite:
# echo gen_sqlite();

// MySQL 8.3:
echo gen_mysql(3308);
// MariaDB 11.7:
# echo gen_mysql(3317);

// PostgreSQL:
#echo gen_pg();