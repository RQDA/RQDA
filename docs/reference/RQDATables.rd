<!-- Generated by pkgdown: do not edit by hand -->
<!DOCTYPE html>
<html lang="en">
  <head>
  <meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1.0">

<title>Data Tables in rqda file — RQDATables • RQDA</title>


<!-- jquery -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.4.1/jquery.min.js" integrity="sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo=" crossorigin="anonymous"></script>
<!-- Bootstrap -->

<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.4.1/css/bootstrap.min.css" integrity="sha256-bZLfwXAP04zRMK2BjiO8iu9pf4FbLqX6zitd+tIvLhE=" crossorigin="anonymous" />

<script src="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.4.1/js/bootstrap.min.js" integrity="sha256-nuL8/2cJ5NDSSwnKD8VqreErSWHtnEP9E7AySL+1ev4=" crossorigin="anonymous"></script>

<!-- bootstrap-toc -->
<link rel="stylesheet" href="../bootstrap-toc.css">
<script src="../bootstrap-toc.js"></script>

<!-- Font Awesome icons -->
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/all.min.css" integrity="sha256-mmgLkCYLUQbXn0B1SRqzHar6dCnv9oZFPEC1g1cwlkk=" crossorigin="anonymous" />
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/v4-shims.min.css" integrity="sha256-wZjR52fzng1pJHwx4aV2AO3yyTOXrcDW7jBpJtTwVxw=" crossorigin="anonymous" />

<!-- clipboard.js -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js" integrity="sha256-inc5kl9MA1hkeYUt+EC3BhlIgyp/2jDIyBLS6k3UxPI=" crossorigin="anonymous"></script>

<!-- headroom.js -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom.min.js" integrity="sha256-AsUX4SJE1+yuDu5+mAVzJbuYNPHj/WroHuZ8Ir/CkE0=" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.headroom.min.js" integrity="sha256-ZX/yNShbjqsohH1k95liqY9Gd8uOiE1S4vZc+9KQ1K4=" crossorigin="anonymous"></script>

<!-- pkgdown -->
<link href="../pkgdown.css" rel="stylesheet">
<script src="../pkgdown.js"></script>




<meta property="og:title" content="Data Tables in rqda file — RQDATables" />
<meta property="og:description" content="The internal data table structures in rqda file, which is a SQLite data base." />




<!-- mathjax -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js" integrity="sha256-nvJJv9wWKEm88qvoQl9ekL2J+k/RWIsaSScxxlsrv8k=" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/config/TeX-AMS-MML_HTMLorMML.js" integrity="sha256-84DKXVJXs0/F8OTMzX4UR909+jtl4G7SPypPavF+GfA=" crossorigin="anonymous"></script>

<!--[if lt IE 9]>
<script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
<script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
<![endif]-->



  </head>

  <body data-spy="scroll" data-target="#toc">
    <div class="container template-reference-topic">
      <header>
      <div class="navbar navbar-default navbar-fixed-top" role="navigation">
  <div class="container">
    <div class="navbar-header">
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      <span class="navbar-brand">
        <a class="navbar-link" href="../index.html">RQDA</a>
        <span class="version label label-default" data-toggle="tooltip" data-placement="bottom" title="Released version">0.3-2</span>
      </span>
    </div>

    <div id="navbar" class="navbar-collapse collapse">
      <ul class="nav navbar-nav">
        <li>
  <a href="../index.html">
    <span class="fas fa-home fa-lg"></span>
     
  </a>
</li>
<li>
  <a href="../reference/index.html">Reference</a>
</li>
      </ul>
      <ul class="nav navbar-nav navbar-right">
        
      </ul>
      
    </div><!--/.nav-collapse -->
  </div><!--/.container -->
</div><!--/.navbar -->

      

      </header>

<div class="row">
  <div class="col-md-9 contents">
    <div class="page-header">
    <h1>Data Tables in rqda file</h1>
    
    <div class="hidden name"><code>RQDATables.rd</code></div>
    </div>

    <div class="ref-description">
    <p>The internal data table structures in rqda file, which is a SQLite data base.</p>
    </div>



    <h2 class="hasAnchor" id="details"><a class="anchor" href="#details"></a>Details</h2>

    <p>Table "annotation" contains file annotations.</p><table class='table'>
<tr><td>fid:</td><td>file id.</td></tr>
<tr><td>position:</td><td>position of annotation.</td></tr>
<tr><td>annotation:</td><td>content of annotation.</td></tr>
<tr><td>owner:</td><td>owner of annotation.</td></tr>
<tr><td>date:</td><td>created date.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>status:</td><td>1 for standard status and 0 for temporarily deleted annotation.</td></tr>
</table>


<p>Table "attributes" contains information about the name list of
  attributes. They are held in the widget of ".AttrNamesWidget".</p><table class='table'>
<tr><td>name:</td><td>name of attributes.</td></tr>
<tr><td>status:</td><td>1 for standard status and 0 for a temporarily deleted attribute.</td></tr>
<tr><td>date:</td><td>created date of an attribute.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>owner:</td><td>owner of an attribute.</td></tr>
<tr><td>memo:</td><td>memo of an attribute. Useful for definition of attributes.</td></tr>
<tr><td>class:</td><td>class of an attribute. It might be "character" or "numeric".</td></tr>
</table>

  
<p>Table "caseAttr" contains information about attributes of cases.</p><table class='table'>
<tr><td>variable:</td><td>name of case attributes, corresponding to name in
    attributes table.</td></tr>
<tr><td>value:</td><td>variable value.</td></tr>
<tr><td>caseID:</td><td>corresponding case id of a variable value.</td></tr>
<tr><td>date:</td><td>created date of a case attribute record.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>owner:</td><td>creator of the case attribute record.</td></tr>
</table>

  
<p>Table "caselinkage" contains information about the relationship
  between case and files of case.</p><table class='table'>
<tr><td>caseid:</td><td>case id.</td></tr>
<tr><td>fid:</td><td>file id.</td></tr>
<tr><td>selfirst:</td><td>beginning position of a text segment associated with a case.</td></tr>
<tr><td>selend:</td><td>ending position of a text segment associated with a case.</td></tr>
<tr><td>status:</td><td>1 for standard status and 0 for temporarily deleted record.</td></tr>
<tr><td>owner:</td><td>creator of the case linkage.</td></tr>
<tr><td>date:</td><td>date of a created case linkage.</td></tr>
<tr><td>memo:</td><td>not used currently.</td></tr>
</table>

  
<p>Table "cases" contains information about case list.</p><table class='table'>
<tr><td>name:</td><td>name of a case.</td></tr>
<tr><td>memo:</td><td>case memo.</td></tr>
<tr><td>owner:</td><td>creator of a case.</td></tr>
<tr><td>date:</td><td>date of creation of a case.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>id:</td><td>case id.</td></tr>
<tr><td>status:</td><td>1 for standard status and 0 for temporarily deleted record.</td></tr>
</table>

  
<p>Table "codecat" contains information about upper-level of code list.</p><table class='table'>
<tr><td>name:</td><td>name of code category.</td></tr>
<tr><td>cid:</td><td>not used currently.</td></tr>
<tr><td>catid:</td><td>id of code category.</td></tr>
<tr><td>owner:</td><td>creator of code category.</td></tr>
<tr><td>date:</td><td>date of creation of code category.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>memo:</td><td>code category memo.</td></tr>
<tr><td>status:</td><td>1 for standard status and 0 for temporarily deleted record.</td></tr>
</table>

  
<p>Table "coding" contains information on codings.</p><table class='table'>
<tr><td>cid :</td><td>code id.</td></tr>
<tr><td>fid :</td><td>file id.</td></tr>
<tr><td>seltext :</td><td>a coding, that is the coded text segment.</td></tr>
<tr><td>selfirst :</td><td>beginning position of the coded text segment.</td></tr>
<tr><td>selend :</td><td>ending position of the coded text segment.</td></tr>
<tr><td>status :</td><td>1 for standard status. 0 for deleted codings (for
  example when a code is deleted, the status of all associated codings
  is set to 0) and -1 for unmarked codings.</td></tr>
<tr><td>owner :</td><td>name of coder or creator of a coding.</td></tr>
<tr><td>date :</td><td>date of creation of a coding.</td></tr>
<tr><td>memo :</td><td>coding memo.</td></tr>
</table>


<p>Table "fileAttr" contains information about attributes of files.</p><table class='table'>
<tr><td>variable:</td><td>character, name of file attribute, corresponding to name in
    attributes table</td></tr>
<tr><td>value:</td><td>value of the file attribute.</td></tr>
<tr><td>fileID:</td><td>corresponding file id of the attribute.</td></tr>
<tr><td>date:</td><td>created date of the file attribute.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>owner:</td><td>creator of the file attribute.</td></tr>
</table>

  
<p>Table "filecat" contains information on the file categorization.</p><table class='table'>
<tr><td>name:</td><td>name of the file category.</td></tr>
<tr><td>fid:</td><td>Not used.</td></tr>
<tr><td>catid:</td><td>if of file category.</td></tr>
<tr><td>owner:</td><td>creator of file-category.</td></tr>
<tr><td>date:</td><td>date of creation of a file category.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>memo:</td><td>file category memo.</td></tr>
<tr><td>status:</td><td>1 for standard status and 0 for temporarily deleted record.</td></tr>
</table>


<p>Table "freecode" contains information on the codes list.</p><table class='table'>
<tr><td>name :</td><td>code name.</td></tr>
<tr><td>memo :</td><td>code memo.</td></tr>
<tr><td>owner :</td><td>creator of a code.</td></tr>
<tr><td>date :</td><td>date of creation of a code.</td></tr>
<tr><td>dateM :</td><td>not used currently.</td></tr>
<tr><td>id :</td><td>code id.</td></tr>
<tr><td>status :</td><td>1 for standard status and 0 for temporarily deleted record.</td></tr>
<tr><td>color:</td><td>color for code marker (added in version 0.19)</td></tr>
</table>


<p>Table "image" contains information about images. It is not used currently.</p>
<p>Table "imageCoding" contains images coding. It is not used currently.</p>  
<p>Table "journal" contains information about field work
  journal. Journal titles are held in widget of ".JournalNamesWidget".</p><table class='table'>
<tr><td>name:</td><td>name of a journal.</td></tr>
<tr><td>journal:</td><td>content of a journal.</td></tr>
<tr><td>date:</td><td>created date of a journal.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>owner:</td><td>owner of a journal.</td></tr>
<tr><td>status:</td><td>1 for standard status and 0 for temporarily deleted journal.</td></tr>
</table>

  
<p>Table "project" contains information about the project and *.rqda file.</p><table class='table'>
<tr><td>encoding:</td><td>not used currently.</td></tr>
<tr><td>databaseversion:</td><td>version of RQDAtables.</td></tr>
<tr><td>date:</td><td>created date of the project.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>memo:</td><td>project memo.</td></tr>
<tr><td>BOM:</td><td>not used currently.</td></tr>
<tr><td>imageDir:</td><td>directory of image. Not used currently.</td></tr>
<tr><td>about:</td><td>meta information about the rqda file.</td></tr>
</table>

  
<p>Table "source" contains the content of files. Files are held in widget
  of ".fnames_rqda".</p><table class='table'>
<tr><td>name:</td><td>name of the file.</td></tr>
<tr><td>id:</td><td>id of the file.</td></tr>
<tr><td>file:</td><td>content of a file.</td></tr>
<tr><td>memo:</td><td>memo of the file.</td></tr>
<tr><td>owner:</td><td>creator the the file.</td></tr>
<tr><td>date:</td><td>the date of the file-import.</td></tr>
<tr><td>dataM:</td><td>date of last editing of the file content.</td></tr>
<tr><td>status:</td><td>1 for standard status and 0 for temporarily deleted file.</td></tr>
</table>

  
<p>The "treecode" table contains information on the codes categorization
  (relationship between codes and the codecat). They are held in widget
  of ".CodeCatWidget". Codes of specific category are held in widget of ".CodeofCat".</p><table class='table'>
<tr><td>cid:</td><td>code id.</td></tr>
<tr><td>catid:</td><td>code category id.</td></tr>
<tr><td>date:</td><td>date of creation of a code categorization.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>memo:</td><td>not used currently.</td></tr>
<tr><td>status:</td><td>1 for standard status and 0 for temporarily deleted
    file.</td></tr>
<tr><td>owner:</td><td>creator the the treecode.</td></tr>
</table>


<p>Table "treefile" contains information about file categorization
  (relation between source files and filecat).</p><table class='table'>
<tr><td>fid:</td><td>file id.</td></tr>
<tr><td>catid:</td><td>file category id.</td></tr>
<tr><td>date:</td><td>date of creation of the file categorization.</td></tr>
<tr><td>dateM:</td><td>not used currently.</td></tr>
<tr><td>memo:</td><td>not used currently.</td></tr>
<tr><td>status:</td><td>1 for standard status and 0 for temporarily deleted
    record.</td></tr>
<tr><td>owner:</td><td>creator the the tree file.</td></tr>
</table>

  

    <h2 class="hasAnchor" id="author"><a class="anchor" href="#author"></a>Author</h2>

    <p>HUANG Ronggui</p>

  </div>
  <div class="col-md-3 hidden-xs hidden-sm" id="pkgdown-sidebar">
    <nav id="toc" data-toggle="toc" class="sticky-top">
      <h2 data-toc-skip>Contents</h2>
    </nav>
  </div>
</div>


      <footer>
      <div class="copyright">
  <p>Developed by Ronggui Huang.</p>
</div>

<div class="pkgdown">
  <p>Site built with <a href="https://pkgdown.r-lib.org/">pkgdown</a> 1.6.1.</p>
</div>

      </footer>
   </div>

  


  </body>
</html>


