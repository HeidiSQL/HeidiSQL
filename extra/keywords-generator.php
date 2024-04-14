<?php

function gen_mysql()
{
    $funcs = file_get_contents('syn-mysql-funcs.txt');
    $funcs = str_replace("\r\n", ',', $funcs);
    $funcs = strtoupper($funcs);
    $funcs = explode(",", $funcs);
    //var_dump($funcs);
    $htmlList = file_get_contents('keywords-mysql.html');
    // <li class="listitem"><p><code class="literal">ACCESSIBLE</code>
    //$htmlList = strtoupper($htmlList);
    preg_match_all('#\<li[^\>]*\>\<p[^\>]*\>\<code[^\>]*\>(\w+)\</code\>#i', $htmlList, $matches);
    //var_dump($matches);
    $keywords = [];
    foreach ($matches[1] as $kw) {
        $kw = strtoupper($kw);
        if(!in_array($kw, $funcs)) {
            $keywords[] = $kw;
        }
    }
    $keywords = array_unique($keywords);
    asort($keywords);
    $keywords = implode(' ', $keywords);
    $keywords = wordwrap($keywords, 73, "\r\n");
    $keywords = str_replace(' ', ',', $keywords);
    $keywords = str_replace("\r\n", ",' +\r\n    '", $keywords);
    return $keywords;
}

echo gen_mysql();