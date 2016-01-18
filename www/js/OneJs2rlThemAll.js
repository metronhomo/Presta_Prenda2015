
////////////////////////////////////////////// Función para layout
function init(){for(var i=0;i<caratulaS.length;i++){var a=caratulaS[i];$("body").append('<div class="jumbotron '+a[0]+' claseTodos" style="background: '+a[1]+'; color: '+a[2]+'; ">'+'<div class="container">'+'<h1>'+a[3]+'</h1>'+'<h3>'+a[4]+'</h3>'+'<p>'+a[5]+'</p>'+'</div>'+'</div>')}$("title").append(tituloDePagina);for(i=0;i<secciones.length;i++){var b=secciones[i];$("#seccionesId").append('<li class='+b[0]+'><a href="#" class="action-button shiny-bound-input" id="'+b[0]+'Action'+'"><p>'+b[1]+'</p></a></li>')}var c=seccionesParaFiltros[0];if(seccionesParaFiltros.length>0)for(i=1;i<seccionesParaFiltros.length;i++){c=c+" "+seccionesParaFiltros[i]}$("#navbar").append('<form class="navbar-form claseTodos '+c+'"><button type="button" class="btn btn-default btn.lg" data-toggle="modal" data-target="#myModal">Filtros</button></form>');for(i=0;i<filtros.length;i++){var d=filtros[i][1];var e=filtros[i][2];var f=filtros[i][3];if(filtros[i][0]=="checkbox"){var g="";for(t=0;t<e.length;t++){g=g+'<label class="checkbox-inline">'+'<input type="checkbox" id="'+f+t+'"checked>'+e[t]+'</label>'}var h="idLabel"+f;var j=''+'<label for="'+h+'">'+d+'</label>'+'<div id="'+h+'">'+g+'</div>';$("#filtrosId").append('<div class="form-group"><ul><li>'+j+'</li></ul></div>')}}}init();
////////////////////////////////////////////// DONT TOUCH //////////////////////////////////////////////
////////////////////////////////////////////// DONT TOUCH //////////////////////////////////////////////
////////////////////////////////////////////// DONT TOUCH //////////////////////////////////////////////
////////////////////////////////////////////// DONT TOUCH //////////////////////////////////////////////
$("#seccionesId").on("click","li",function(){$("#seccionesId > li.active").removeClass("active");$(this).addClass("active");var a=$("#seccionesId > li.active").attr("class").split(" ")[0];$(".claseTodos").hide();for(i=0;i<secciones.length;i++){if(secciones[i][0]===a){var b=secciones[i][2]}};b="."+b;$(b).show();masterr();$(window).resize()});$(window).load(function(){$(".claseTodos").hide();$("#seccionesId > li.SA").addClass("active");$(".SAClase").show();setTimeout(function(){masterr()},1000);$(window).resize()});                                                                                             ////////
////////////////////////////////////////////// DONT TOUCH //////////////////////////////////////////////
////////////////////////////////////////////// DONT TOUCH //////////////////////////////////////////////
////////////////////////////////////////////// DONT TOUCH //////////////////////////////////////////////
////////////////////////////////////////////// DONT TOUCH //////////////////////////////////////////////

function creaContainer(dondevoy,miClase,miTitulo,miSubtitulo,miIdContenido,miContenido){$(dondevoy).append('<div class="container '+miClase+' claseTodos">'+'<div class="page-header">'+'<h2>'+miTitulo+'</h2>'+'</div>'+'<p class="lead">'+miSubtitulo+'</p>'+'<div id="'+miIdContenido+'" class="container">'+miContenido+'</div>'+'</div>')};

function creaContainerMultiple(dondevoy,miClase,cuantasCeldas,miIdContenidoOriginal){var mimodular=cuantasCeldas%2;if(mimodular===0){var cuantosRow=cuantasCeldas/2}else if(mimodular===1){var cuantosRow=Math.round(cuantasCeldas/2)+1}var misRows='';var mitdId1='';var mitdId2='';for(t=0;t<cuantosRow;t++){mitdId1=t+"A";mitdId2=t+"B";misRows=misRows+'<div class="row miRows"><div class="col-md-6" id="'+miIdContenidoOriginal+mitdId1+'"></div><div class="col-md-6" id="'+miIdContenidoOriginal+mitdId2+'"></div></div>'}$(dondevoy).append(misRows)}

$.makeTable=function(mydata){var table=$('<table class="table table-hover">');var tblHeader="<thead><tr>";for(var k in mydata[0])tblHeader+="<th class='text-center'>"+k+"</th>";tblHeader+="</tr></thead>";$(tblHeader).appendTo(table);$.each(mydata,function(index,value){var TableRow="<tr>";$.each(value,function(key,val){TableRow+="<td class='text-center'>"+val+"</td>"});TableRow+="</tr>";$(table).append(TableRow)});return($(table))};

function creaTabla(cNTabidObjetivo,cNTabpalabras){$(cNTabidObjetivo).addClass("table-responsive table-full-width table-condensed");var table=$.makeTable(cNTabpalabras);$(cNTabidObjetivo).empty().append(table)}

function creaColumna(cUBSidG,cUBSTitulo,cUBSSubtitulo,cUBSTEjex,cUBSTEjey,cUBSDatos,cUBSTotal){function dameCasos(elNombre){for(var i=0;i<cUBSDatos.length;i++){if(cUBSDatos[i].name==elNombre){return(cUBSDatos[i].casos)}}}var casosTotal=cUBSTotal;var cUBS4Chart=[];for(i=0;i<cUBSDatos.length;i++){cUBS4Chart.push({name:cUBSDatos[i].name,data:cUBSDatos[i].data})}if(casosTotal>2100){var newcSubtitulo=cUBSSubtitulo}else{var newcSubtitulo=cUBSSubtitulo+'<br><h4><small>(Total de casos: '+casosTotal+')</small></h>'}var cUBSColorz=[];for(i=0;i<cUBSDatos.length;i++){cUBSColorz.push(String(cUBSDatos[i].color))}var options={chart:{renderTo:cUBSidG,type:'column',height:400},title:{text:cUBSTitulo,style:{fontSize:'14px'}},subtitle:{text:newcSubtitulo,style:{fontSize:'10px'}},xAxis:{categories:[cUBSTEjex]},credits:{enabled:false},yAxis:{min:0,max:100,title:{text:cUBSTEjey},labels:{overflow:'justify',format:'{value}'}},legend:{itemStyle:{fontSize:10},symbolHeight:10,symbolWidth:10,},tooltip:{formatter:function(){return'<table><tr><td style="color:'+this.series.color+';padding:0">'+this.series.name+': </td>'+'<td style="padding:0"><b> '+this.y+' %</b></td></tr>'+'<tr><td colspan = "2" style="text-align: center">('+dameCasos(this.series.name)+' casos)</td></tr></table>'},useHTML:true},plotOptions:{column:{colorByPoint:false,pointPadding:0.2,borderWidth:0,dataLabels:{enabled:true,format:'<b>{point.y}</b>'}}},colors:cUBSColorz,series:cUBS4Chart};var chart=new Highcharts.Chart(options)}

function creaColumnaAutoColor(cUBSidG,cUBSTitulo,cUBSSubtitulo,cUBSTEjex,cUBSTEjey,cUBSDatos,cUBSTotal){function dameCasos(elNombre){for(var i=0;i<cUBSDatos.length;i++){if(cUBSDatos[i].name==elNombre){return(cUBSDatos[i].casos)}}}var casosTotal=cUBSTotal;var cUBS4Chart=[];for(i=0;i<cUBSDatos.length;i++){cUBS4Chart.push({name:cUBSDatos[i].name,data:cUBSDatos[i].data})}if(casosTotal>2100){var newcSubtitulo=cUBSSubtitulo}else{var newcSubtitulo=cUBSSubtitulo+'<br><h4><small>(Total de casos: '+casosTotal+')</small></h>'}var espacio=cUBS4Chart.length;espacio=Math.round(Math.pow(.94,espacio)*24);var options={chart:{renderTo:cUBSidG,type:'column'},title:{text:cUBSTitulo,style:{fontSize:'14px'}},subtitle:{text:newcSubtitulo,style:{fontSize:'10px'}},xAxis:{categories:[cUBSTEjex]},credits:{enabled:false},yAxis:{min:0,max:100,title:{text:cUBSTEjey},labels:{overflow:'justify',format:'{value}'}},legend:{itemStyle:{fontSize:10},symbolHeight:10,symbolWidth:10,},tooltip:{formatter:function(){return'<table><tr><td style="color:'+this.series.color+';padding:0">'+this.series.name+': </td>'+'<td style="padding:0"><b> '+this.y+' %</b></td></tr>'+'<tr><td colspan = "2" style="text-align: center">('+dameCasos(this.series.name)+' casos)</td></tr></table>'},useHTML:true},plotOptions:{column:{colorByPoint:false,pointPadding:0.2,borderWidth:0,dataLabels:{enabled:true,format:'<b>{point.y}</b>'}}},series:cUBS4Chart};var chart=new Highcharts.Chart(options)}

function creaColumnaApilada(cUBSidG,cUBSTitulo,cUBSSubtitulo,cUBSTEjex,cUBSTEjey,cUBSDatos,cUBSColorz,paseCual){if(paseCual===1){var cUBS4Chart=[];for(i=0;i<cUBSDatos.length;i++){cUBS4Chart.push({name:cUBSDatos[i].name,data:cUBSDatos[i].data});var options={chart:{renderTo:cUBSidG,type:'column'},title:{text:cUBSTitulo,style:{fontSize:'14px'}},subtitle:{text:cUBSSubtitulo,style:{fontSize:'10px'}},xAxis:{categories:cUBSTEjex},credits:{enabled:false},yAxis:{min:0,max:100,title:{text:cUBSTEjey},labels:{overflow:'justify',format:'{value}'}},legend:{itemStyle:{fontSize:10},symbolHeight:10,symbolWidth:10,},tooltip:{headerFormat:'<span style="font-size:10px">{point.key}</span><table>',pointFormat:'<tr><td style="color:{series.color};padding:0">{series.name}: </td>'+'<td style="padding:0"><b>{point.y:.0f} %</b></td></tr>',footerFormat:'</table>',shared:true,useHTML:true},plotOptions:{column:{stacking:'normal',colorByPoint:false,pointPadding:0.2,borderWidth:0,dataLabels:{enabled:true,format:'<b>{point.y}</b>'}}},colors:cUBSColorz,series:cUBS4Chart}}}else{var cUBS4Chart=[];for(i=0;i<cUBSDatos.length;i++){cUBS4Chart.push({name:cUBSDatos[i].name,data:cUBSDatos[i].casos})}var options={chart:{renderTo:cUBSidG,type:'column'},title:{text:cUBSTitulo,style:{fontSize:'14px'}},subtitle:{text:cUBSSubtitulo,style:{fontSize:'10px'}},xAxis:{categories:cUBSTEjex},credits:{enabled:false},yAxis:{min:0,title:{text:'Casos'},labels:{overflow:'justify',format:'{value}'}},legend:{itemStyle:{fontSize:10},symbolHeight:10,symbolWidth:10,},tooltip:{headerFormat:'<span style="font-size:10px">{point.key}</span><table>',pointFormat:'<tr><td style="color:{series.color};padding:0">{series.name}: </td>'+'<td style="padding:0"><b>{point.y:.0f} casos</b></td></tr>',footerFormat:'</table>',shared:true,useHTML:true},plotOptions:{column:{stacking:'normal',colorByPoint:false,pointPadding:0.2,borderWidth:0,dataLabels:{enabled:true,format:'<b>{point.y}</b>'}}},colors:cUBSColorz,series:cUBS4Chart}}var chart=new Highcharts.Chart(options)}

function creaABeneficios(cUBSidG,cUBSTitulo,cUBSSubtitulo,cUBSDatos){var ejeX=cUBSDatos.ejeX;var cUBMversion=cUBSDatos.cUBMversion;var colorz=cUBSDatos.colores;var datal=cUBSDatos.datos;var cUBS4Chart=[];for(i=0;i<datal.length;i++){cUBS4Chart.push({name:datal[i].name,data:datal[i].data})}console.log(cUBS4Chart);if(cUBMversion==1){$(function(){$(cUBSidG).highcharts({chart:{type:'column'},title:{text:cUBSTitulo,style:{fontSize:'14px'}},subtitle:{text:cUBSSubtitulo,style:{fontSize:'10px'}},xAxis:{categories:ejeX,crosshair:true},credits:{enabled:false},yAxis:{min:0,max:100,title:{text:"Porcentaje"},labels:{overflow:'justify',format:'{value} %'}},legend:{itemStyle:{fontSize:8},symbolHeight:8,symbolWidth:8,},tooltip:{headerFormat:'<span style="font-size:10px">{point.key}</span><table>',pointFormat:'<tr><td style="color:{series.color};padding:0">{series.name}: </td>'+'<td style="padding:0"><b>{point.y:.0f} %</b></td></tr>',footerFormat:'</table>',shared:true,useHTML:true},plotOptions:{column:{colorByPoint:false,pointPadding:0.2,borderWidth:0,dataLabels:{enabled:true,format:'<b style="font-size:8px">{point.y}</b>'}}},colors:colorz,series:cUBS4Chart})})}else{$(function(){$(cUBSidG).highcharts({chart:{type:'column'},title:{text:cUBSTitulo,style:{fontSize:'14px'}},subtitle:{text:cUBSSubtitulo,style:{fontSize:'10px'}},xAxis:{categories:ejeX,crosshair:true},credits:{enabled:false},yAxis:{min:0,max:2100,title:{text:"Casos"},labels:{overflow:'justify',format:'{value}'}},legend:{itemStyle:{fontSize:8},symbolHeight:8,symbolWidth:8,},tooltip:{headerFormat:'<span style="font-size:10px">{point.key}</span><table>',pointFormat:'<tr><td style="color:{series.color};padding:0">{series.name}: </td>'+'<td style="padding:0"><b>{point.y:.0f} casos</b></td></tr>',footerFormat:'</table>',shared:true,useHTML:true},plotOptions:{column:{colorByPoint:false,pointPadding:0.2,borderWidth:0,dataLabels:{enabled:true,format:'<b style="font-size:8px">{point.y}</b>'}}},colors:colorz,series:cUBS4Chart})})}}

function destroy(donde){$(donde).html('');}

function runMasterr(maestro){
  $(".miRows").html("");
  for( var iM=0; iM<maestro.length; iM++){  
    if(maestro[iM][0]=="creaContainer"){
      // Creo un contaiMner
      creaContainer(maestro[iM][1],maestro[iM][2], maestro[iM][3], maestro[iM][4], maestro[iM][5], maestro[iM][6] );
    }else
    if(maestro[iM][0]=="creaColumna"){
      // Creo un contaiMner
      creaColumna(maestro[iM][1],maestro[iM][2],maestro[iM][3],maestro[iM][4],maestro[iM][5],maestro[iM][6],maestro[iM][7]);
    }else
    if(maestro[iM][0]=="creaColumnaAutoColor"){
      // Creo un contaiMner
      creaColumnaAutoColor(maestro[iM][1],maestro[iM][2],maestro[iM][3],maestro[iM][4],maestro[iM][5],maestro[iM][6],maestro[iM][7]);
    }else
    if(maestro[iM][0]=="creaContainerMultiple"){
      // Creo un contaiMner
      creaContainerMultiple(maestro[iM][1], maestro[iM][2], maestro[iM][3], maestro[iM][4]);
    }else
    if(maestro[iM][0]=="creaTabla"){
      // Creo un contaiMner
      creaTabla(maestro[iM][1], maestro[iM][2]);
    }
    if(maestro[iM][0]=="destroy"){
      // Creo un contaiMner
      destroy(maestro[iM][1]);
    }
    if(maestro[iM][0]=="creaABeneficios"){
      // Creo un contaiMner
      creaABeneficios(maestro[iM][1],maestro[iM][2],maestro[iM][3],maestro[iM][4]);
    }
    if(maestro[iM][0]=="creaColumnaApilada"){
      // Creo un contaiMner
      creaColumnaApilada(maestro[iM][1],maestro[iM][2],maestro[iM][3],maestro[iM][4], maestro[iM][5],maestro[iM][6],maestro[iM][7],maestro[iM][8]);
    }
  }
};

runMasterr(jedi);
$("body").append('<div class="shiny-image-output SEClase claseTodos" id="graficaSE3"></div>');
