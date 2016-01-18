// elementos

var tituloDePagina = "Presta Prenda";

// Añadir tantas secciones como se desee
// seccion1 = ["nombreSelector","nombrePantalla","nombreClase","icono"]
// seccion2 = ["nombreSelector2","nombrePantalla2", "nombreClase2","icono2"]

/// Recuerda: "nombreClase" es para los objetos (páneles, filtros) que se activarán
/// cuando se le de click a la acción, es muy importante no perdér de vista estas clases

/// Recuerda: el "selector" es para la función de JQuery más abajo,
/// mientras que "la clase" es para nombres los objetos (pàneles, filtros)

var secciones = [
    ["SA","Objetivos","SAClase"],
    ["SB","Incidencias & Metodología","SBClase"],
    ["SC","Bancarización","SCClase"],
    ["SD","Conocimiento y uso","SDClase"],
    ["SE","Publicidad","SEClase"],
    ["SF","Usos y Hábitos","SFClase"],
    ["SG","Empeños recientes","SGClase"],
    ["SH","Imagen de marca","SHClase"],
    ["SI","Imagen Presta Prenda","SIClase"],
    ["SJ","Contacto","SJClase"]
];

/// Hago un array con las carátulas que quiero agregar
var caratulaS =[
["SAClase", "#061826", "#A7A2A9", "Presta Prenda 2015", "", "<hr><h3>Objetivo General</h3><ul><li>Identificar los hábitos de consumo y preferencia en los que incurren los usuarios de la categoría de Préstamos Prendarios.</li></ul><h3>Objetivos Específicos</h3><ul><li>Entender los usos y hábitos de compra que se tienen en la categoría: ¿Quién?, ¿Qué? </li><li> Brand Awareness : Top of mind y Share of mind.</li> <li> Barreras, Drivers y Triggers hacía la categoría. </li><li> Uso del servicio:<ul<li>Elección de la frecuencia de pago: semanal, quincenal </li><li>Duración del préstamo</li><li>Entendimiento del % de interés que pagan por el préstamo</li><li>Preferencia y elección de marca</li><li>Abandono</li></ul></li><li>Identificar los atributos con mayor importancia dentro de la categoría.</li><li>Recordación publicitaria y medios.</li><li>Imagen de “Presta Prenda” y competencia.</li><li>Identificar qué tan ligado debe estar el servicio de Préstamos Prendarios a Banco Azteca. </li></ul>"]
];

// Añadir tantos filtros como se desee
// ["tipoForm", "etiquetaForm", "[ArrayConOpciones]","idDelForm"],
// por ahora funcionan los user form tipo: checkbox, radio y select 

/// Declaro para qué secciones van a servir los filtros. Son los mismos
/// que escribiste en las secciones. i.e. Los filtros estarán presentes
/// para las secciones que aquí se pongan.

var seccionesParaFiltros = ["SCClase", "SDClase", "SEClase", "SFClase", "SGClase", "SHClase", "SIClase"];

var filtros = [
//  ["select", "filtro por sexo:", ["Sin filtro", "Mujer", "Hombre"],"formSexo"],
  ["checkbox", "filtro por sexo:",["Mujer", "Hombre"],"formSexo"],
  ["checkbox", "filtro por edad:",["24 - 34", "35 - 44", "45 - 54", "55 - 60"],"formEdad"],
  ["checkbox", "filtro por NSE:",["C", "C-", "D+", "D"],"formNSE"],
  ["checkbox", "filtro por artículos:",["Oro", "Relojes o diamantes", "Coches", "Otras mercancias"],"formObj"],
  ["checkbox", "filtro por zonas:",["Golfo", "AMCM", "Norte", "Centro", "Sureste", "Occidente"],"formZona"],
  ["checkbox", "filtro por última vez:",["Presta Prenda", "First Cash", "Fundación Rafael Dondé", "Nacional Monte de Piedad", "Prendamex", "Otros"],"formFirma"]
];

/// Pasame los array para evaluar
// creaContainer(dondevoy, miClase, miTitulo, miSubtitulo, miIdContenido, miContenido)
// creaTablasContainer(dondevoy, miClase, cuantasCeldas ,miIdContenidoOriginal)

function selectorFilaColumnaCasos(miSelect){
  return("<select class='form-control' id='"+miSelect+"'><option value=1>Porcentaje de Fila</option><option value=2>Porcentaje de Columna</option><option value=3>Total de casos</option></select>");
}
function selectorPorcentajeCasos(miSelect){
  return("<select class='form-control' id='"+miSelect+"'><option value=1>Porcentaje suma 100%</option><option value=2>Porcentaje del total de personas</option></select>");
}
function selectoMultiple(miSelect){
  return("<select class='form-control' id='"+miSelect+"'><option value=1>Total</option><option value=2>Cruzar por sexo</option><option value=3>Cruzar por edad</option><option value=4>Cruzar por NSE</option><option value=5>Cruzar por artículos</option><option value=6>Cruzar por zona</option><option value=7>Cruzar por última vez</option></select>");
}
function selectorPCYM(miSelect){
  var miSelect1= miSelect + "A"
  var miSelect2= miSelect + "B"
  return(selectorPorcentajeCasos(miSelect1) +  selectoMultiple(miSelect2));
}
function selectorMarca1(miSelect){
  return("<select class='form-control' id='"+miSelect+"'><option value=1>Total</option><option>Nacional Monte de Piedad</option><option>Presta Prenda </option><option>First Cash</option><option>Prendamex</option><option>Fundación Rafael Dondé</option></select>");
}
function selectorMarca2(miSelect){
  return("<select class='form-control' id='"+miSelect+"'><option value=1>Total</option><option>Nacional Monte de Piedad</option><option>Presta Prenda</option><option>First Cash</option><option> Prendamex</option><option>Fundación Rafael Dondé</option></select>");
}
function selectorRecientes(miSelect){
  return("<select class='form-control' id='"+miSelect+"'><option value=1>Total</option><option value=2>Usados últimos 12 meses</option></select>");
}
function selectorFilaColumnaCasosA(miSelect){
  return("<select class='form-control' id='"+miSelect+"'><option value=1>Total de casos</option><option value=2>Porcentaje de Columna</option></select>");
}
function selectorFilaColumnaCasosB(miSelect){
  return("<select class='form-control' id='"+miSelect+"'><option value=1>Porcentaje</option><option value=2>Numero de casos</option></select>");
}

var ptext = '<h4><small>Hallazgos:<ul><li><b>Nacional Monte de Piedad</b> mantiene una percepción moderada en la mayoría de los atributos. Es la única marca que se apropia del concepto “intereses bajos”. El público NO la asocia con tener un monedero electónico. Su estabilidad en cuanto a la percepción del público la separa de manera significativa de las demás marcas definiéndola como la marca líder de la categoría.</li> <li><b>Prendinero</b> aparece como una marca muy diferenciada, sin embargo esto se explica por la alta variabilidad de sus datos. Se apropia de la característica “personal amable”. La límitada disponibilidad de sucursales también es un factor diferenciador para la marca. Todo esto unido con tener una buena asociación con el atributo de asesoría personalizada la convierten en una marca cercana a la gente.</li> <li><b>Prestaprenda</b> tiene dos diferenciadores principales. El primero de estos es el enfoque en el empeño de oro y el segundo la disponibilidad de monederos electrónicos. Junto con Rafael Dondé es la marca más estable en la percepción del público después de monte de piedad. Esto la convierte en una marca competidora para el líder.</li> <li><b>Firstcash</b> se distingue bastante de las demás. Apropiándose del atributo “se puede empeñar cualquier artículo”. Esto va de la mano con una alta asociación con tener requisitos fáciles y un proceso de empeño fácil. Y se complementa con la falta de asociación con los servicios de asesoría y disponibilidad de monedero electrónico. Lo cuál la convierte en la marca de empeño express por así decirlo.</li> <li><b>Rafael Dondé</b> es la marca menos diferenciada en la percepción del público. Esto a pesar de contar con una asociación moderadamente positiva en personal amable y facilidades para recuperar el artículo. Es una marca genérica.</li> <li><b>Prendamex</b> es una marca “parecida” a firstcash en la mente del público, sin embargo no logra sobresalir como dicha marca. Sin embargo es una marca que también es de empeño rápido.</li> </h4></small>'
//var ptext='';

var jedi = [
["creaContainer", "body", "SBClase", "Incidencias", "", "", "<ul><li>Se realizó un total de 9,111 contactos (que pasaron todos los filtros) para lograr las 2,100 entrevistas de personas que han empeñado</li><li>Es decir, en promedio 1 de cada 4 personas de NSE C,C-,D y D+ ha empeñado en los últimos 12 meses</li></ul>"],
["creaContainer", "body", "SBClase", "Metodología", "<h3><small>Levantamiento de campo: 17 de noviembre al 14 de diciembre  del 2015</small></h3>", "", "<img class='img-responsive img-rounded center-block' src='img/metodologia1.png'><br><img class='img-responsive img-rounded center-block' src='img/metodologia2.png'>"],
["creaContainer", "body", "SBClase", "Perfiles por tipo de artículo que empeña", "<blockquote class='bg-success'>El oro es empeñado en mayor medida por mujeres, por usuarios correspondientes a los NSE C- y D+ y en la zona del Sureste</blockquote>", "graficaSB1", ""], 
  ["creaContainer", "#graficaSB1", "SBClase", "Artículos por sexo", selectorFilaColumnaCasos("graficaSB11Select"), "graficaSB11", ""],
  ["creaContainer", "#graficaSB1", "SBClase", "Artículos por edad", selectorFilaColumnaCasos("graficaSB12Select"), "graficaSB12", ""], 
  ["creaContainer", "#graficaSB1", "SBClase", "Artículos por NSE", selectorFilaColumnaCasos("graficaSB13Select"), "graficaSB13", ""], 
  ["creaContainer", "#graficaSB1", "SBClase", "Artículos por Zona", selectorFilaColumnaCasos("graficaSB14Select"), "graficaSB14", ""], 
  ["creaContainer", "#graficaSB1", "SBClase", "Artículos por lugar que usó la última vez", selectorFilaColumnaCasos("graficaSB15Select"), "graficaSB15", ""],

["creaContainer","body","SCClase", "Total de usuarios", "<blockquote class='bg-success'>El 55% de los usuarios de Préstamos Prendarios son bancarizados, siendo “Tarjeta de Nómina” el producto con el que la mayoría cuenta.</blockquote><small>Resultados para toda esta sección: </small>", "graficaSC1", ""],
  ["creaContainer", "#graficaSC1", "SCClase", "Cuentan con un producto bancario",selectoMultiple("graficaSC1s1Select"), "graficaSC1s1", ""],
  ["creaContainer", "#graficaSC1", "SCClase", "Productos bancarios con los que cuentan", selectoMultiple("graficaSC1s2Select"), "graficaSC1s2", ""], 
["creaContainer","body","SCClase", "Usuarios de Tarjeta de nómina", selectoMultiple("graficaSC2Select"), "graficaSC2", ""],
["creaContainer","body","SCClase", "Usuarios de Tarjeta de débito", selectoMultiple("graficaSC3Select"), "graficaSC3", ""],
["creaContainer","body","SCClase", "Usuarios de Tarjeta de crédito", selectoMultiple("graficaSC4Select"), "graficaSC4", ""],
["creaContainer","body","SCClase", "Usuarios de Cuenta de Ahorro", selectoMultiple("graficaSC5Select"), "graficaSC5", ""],
["creaContainer","body","SCClase", "Usuarios de Cuenta de cheques", selectoMultiple("graficaSC6Select"), "graficaSC6", ""],
["creaContainer","body","SCClase", "Usuarios de Seguros", selectoMultiple("graficaSC7Select"), "graficaSC7", ""],
["creaContainer","body","SCClase", "Usuarios de Afore", selectoMultiple("graficaSC8Select"), "graficaSC8", ""],
["creaContainer","body","SCClase", "Usuarios de Crédito hipotecario", selectoMultiple("graficaSC9Select"), "graficaSC9", ""],
["creaContainer","body","SCClase", "Usuarios de Crédito automotriz", selectoMultiple("graficaSC10Select"), "graficaSC10", ""],
["creaContainer","body","SCClase", "Usuarios de Crédito empresarial", selectoMultiple("graficaSC11Select"), "graficaSC11", ""],
["creaContainer","body","SCClase", "Usuarios de Préstamo personal", selectoMultiple("graficaSC12Select"), "graficaSC12", ""],

["creaContainer","body","SDClase", "<b>Top of Mind</b>", "<blockquote class='bg-success'>Nacional Monte de Piedad es la marca que cuenta con mayor recordación de nombre  de manera espontánea y ayudada, siguiendo Presta Prenda</blockquote><p>Primer mención espontánea</p>"+selectoMultiple("graficaSD1Select"), "graficaSD1", ""],
["creaContainer","body","SDClase", "<b>Share of mind</b>", "<p>Todas las menciones espontáneas</p>"+selectoMultiple("graficaSD1ASelect"), "graficaSD1A",""],
["creaContainer","body","SDClase", "<b>Conocimiento Total</b>", "<p>Todas las menciones espontáneas más las ayudadas</p>"+selectoMultiple("graficaSD2Select"), "graficaSD2",""],
["creaContainer","body","SDClase", "<b>Lugares que utilizó alguna vez</b>", "<blockquote class='bg-success'>Presta Prenda y First Cash tienen un comportamiento de uso similar tanto en los últimos 12 meses, alguna vez y por última vez, después del líder de la categoría (Monte de Piedad).</blockquote><p>Respuesta con ayuda</p>"+selectoMultiple("graficaSD3Select"), "graficaSD3",""],
["creaContainer","body","SDClase", "<b>Lugares que utilizó en los últimos 12 meses</b>", "<p>Respuesta con ayuda</p>"+selectoMultiple("graficaSD4Select"), "graficaSD4",""],
["creaContainer","body","SDClase", "<b>Lugar que utilizó la última vez</b>", selectoMultiple("graficaSD5Select"), "graficaSD5",""],
["creaContainer","body","SDClase", "<b>Funnel</b>", "<h3><small>(Esta gráfica es estática)</small></h3>", "","<img class='img-responsive img-rounded center-block' src='img/funnel.png'>"],
["creaContainer","body","SDClase", "<b>Funnel de Oro</b>", "<h3><small>(Esta gráfica es estática)</small></h3>", "","<img class='img-responsive img-rounded center-block' src='img/funnel2.png'>"],
["creaContainer", "body", "SDClase", "<b>Abandono</b>", "<blockquote class='bg-success'> De forma declarada resulta ser que los principales motivos de abandono de una casa de empeño son: “Los intereses son muy altos”, “Lejanía de las sucursales”</blockquote><h4><small>Marcas que ha usado alguna vez, y que no ha vuelto a usar en los últimos 12 meses</small></h4>", "graficaSDB", ""], 
  ["creaContainer", "#graficaSDB", "SDClase", "Lugares abandonados", selectoMultiple("graficaSDB11Select"), "graficaSDB1", ""],
  ["creaContainer", "#graficaSDB", "SDClase", "Razones de abandono", "<h3><small>¿Por qué dejó de utilizar a ... para obtener sus préstamos prendarios? (Motivos)</small></h3>"+selectorMarca1("graficaSDB12Select"), "graficaSDB2", ""],

["creaContainer","body","SEClase", "<b>Share Publicidad</b>", "<blockquote class='bg-success'>De manera espontánea Monte de Piedad y Presta Prenda mantienen su posición de líderes en cuanto a recordación Publicitaria, mientras que Prendamex y Fundación Rafael Dondé  aparecen consecutivamente</blockquote><p>Espontáneo</p>"+selectoMultiple("graficaSE1Select"), "graficaSE1", ""],
["creaContainer","body","SEClase", "<b>Publicidad Total</b>", "<p>Share + Ayudado</p>"+selectoMultiple("graficaSE2Select"), "graficaSE2", ""],
["creaContainer","body","SEClase", "<b>Publicidad Medios a Total</b>", "<p>Share + Ayudado</p>"+selectoMultiple("graficaSE2VSelect"), "graficaSE2V", ""],
["creaContainer","body","SEClase", "<b>Publicidad Medios por marca</b>", "<blockquote class='bg-success'>Fueron cinco los principales medios por los cuales la gente recuerda publicidad  de la categoría; es importante destacar que el 70% de la recordación se centra en tan solo dos medios</blockquote><h3><small>¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?</small></h3><h3><small>De estos lugares para empeñar artículos que le voy a mostrar ¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?</small></h3>"+
                    '<div><select id="graficaSE3Select"><option>Total</option><option>Nacional Monte de Piedad</option><option>Presta Prenda</option><option>First Cash</option><option>Fundación Rafael Dondé</option><option>Prendamex</option></select></div>'+
                    '<div><select id="graficaSE3Select2"><option value=1>Espontáneo&Ayudado</option><option value=2>Espontáneo</option><option value=3>Ayudado</option></select></div>'+
                    '<div><label class="checkbox-inline"><input type="checkbox" id="graficaSE31" checked="checked">Televisión</label>'+
                    '<label class="checkbox-inline"><input type="checkbox" id="graficaSE32" checked="checked">Revista</label>'+
                    '<label class="checkbox-inline"><input type="checkbox" id="graficaSE33" checked="checked">Camiones</label>'+
                    '<label class="checkbox-inline"><input type="checkbox" id="graficaSE34" >Metro</label>'+
                    '<label class="checkbox-inline"><input type="checkbox" id="graficaSE35" >Pantallas Elektra/Banco Azteca</label>'+
                    '<label class="checkbox-inline"><input type="checkbox" id="graficaSE36" checked="checked">Posters</label>'+
                    '<label class="checkbox-inline"><input type="checkbox" id="graficaSE37" >Internet</label>'+
                    '<label class="checkbox-inline"><input type="checkbox" id="graficaSE38" >Redes Sociales</label>'+
                    '<label class="checkbox-inline"><input type="checkbox" id="graficaSE39" checked="checked">Radio</label>'+
                    '<label class="checkbox-inline"><input type="checkbox" id="graficaSE40" >Otros</label></div>', "", ""],

["creaContainer","body","SFClase", "<b>Razones para empeñar</b>", "<blockquote class='bg-success'>El préstamo adquirido es para cubrir gastos inmediatos, principalmente relacionados con: <ol><li>Necesidades básicas (despensa)</li><li>Pago de servicios</li><li>Emergencias Médicas</blockquote><p>Espontáneo</p>"+selectoMultiple("graficaSF1Select"), "graficaSF1", ""],
["creaContainer","body","SFClase", "<b>Lugar que utilizó la última vez</b>", "<p>Share</p>"+selectoMultiple("graficaSF2Select"), "graficaSF2",""],
["creaContainer","body","SFClase", "<b>Razones para empeñar en ... </b>", "<blockquote class='bg-success'>“Cercanía de sucursales”, “ Mayor monto de préstamo” y “Los intereses bajos” son las principales razones por las cuales han elegido una casa de empeño la última vez</blockquote><h3><small>¿Cuáles son las razones por las que empeñó en este lugar? (última vez)</small></h3>"+selectorMarca2("graficaSF3Select"), "graficaSF3",""],
["creaContainer","body","SFClase", "<b>Motivos para confiar en ... </b>", "<blockquote class='bg-success'>Los motivos para confiar en su último lugar de empeño son:<ul><li>Para Monte de Piedad, Presta Prenda y Prendamex: El ser empresas reconocidas</li><li>First Cash:  Intereses bajos</li><li>Fundación Rafael Dondé:  El cuidado y seguridad de las prendas</li></ul></blockquote><h3><small>¿Cuáles son los motivos que le dan confianza para empeñar en este lugar? (última vez)</small></h3>"+selectorMarca2("graficaSF4Select"), "graficaSF4",""],

["creaContainer","body","SGClase", "<b>Empeños activos</b>", selectoMultiple("graficaSG1Select"), "graficaSG1", ""],
["creaContainer","body","SGClase", "<b>Empeños recientes</b>", "<h3><small>Empeños realizados en los últimos 12 meses</small></h3>", "graficaSGA", ""],
["creaContainer","#graficaSGA","SGClase", "<b>Lugares que utilizó en los últimos 12 meses</b>", "<p>Respuesta con ayuda</p>", "graficaSG2",""],
["creaContainer","#graficaSGA","SGClase", "<b>Meses desde el préstamo</b>", "<blockquote class='bg-success'>La mayoría de los usuarios ha obtenido su préstamo entre los meses de agosto y septiembre de 2015</blockquote>"+selectorRecientes("graficaSG3Select"), "graficaSG3",""],
["creaContainer","#graficaSGA","SGClase", "<b>Artículos empeñados</b>", "<blockquote class='bg-success'>Las prendas más empeñadas son: <ol><li>Oro</li><li>Ceulares</li><li>Artículos electrónicos</li></blockquote>"+selectorRecientes("graficaSG4Select"), "graficaSG4",""],
["creaContainer","#graficaSGA","SGClase", "<b>Monto prestado por su artículo</b>", "<blockquote class='bg-success'>Entre los usuarios el monto de préstamo oscila en un rango de $500 a $1,000</blockquote>"+selectorRecientes("graficaSG5Select"), "graficaSG5",""],
//["creaContainer","#graficaSGA","SGClase", "<b>Monto pagado por su artículo</b>", ""+selectorRecientes("graficaSG6Select"), "graficaSG6",""],
["creaContainer","#graficaSGA","SGClase", "<b>Interés del préstamo</b>", "<blockquote class='bg-success'>El 48% de los usuarios declaran -No saber- cuál es la tasa de interés que pagan por su préstamo</blockquote><h3><small>Recordación</small></h3>"+selectorRecientes("graficaSG7Select"), "graficaSG7",""],
["creaContainer","#graficaSGA","SGClase", "<b>Costo del préstamo</b>", "<blockquote class='bg-success'>De acuerdo al monto recibido y monto pagado declarado por los usuarios, el costo del préstamo está entre un rango del 10% - 15%</blockquote><h3><small>Variación porcentual entre monto recibido y monto pagado por artículo</small></h3>"+selectorRecientes("graficaSG8Select"), "graficaSG8",""],
["creaContainer","#graficaSGA","SGClase", "<b>Frecuencia de pago</b>", "<blockquote class='bg-success'>La frecuencia de pago más frecuente para realizar su pago es -Mensualmente-, pagando la totalidad del préstamo, de los cuales más de la mitad (56%) no recurren a refrendos</blockquote>"+selectorRecientes("graficaSG9Select"), "graficaSG9",""],
["creaContainer","#graficaSGA","SGClase", "<b>Forma de pago</b>", selectorRecientes("graficaSG10Select"), "graficaSG10",""],
["creaContainer","#graficaSGA","SGClase", "<b>Refrendos</b>", selectorRecientes("graficaSG11Select"), "graficaSG11",""],

["creaContainer","body","SHClase", "Opciones de préstamo", "<blockquote class='bg-success'>La mayoría de los entrevistados (84 %) solamente consideran un lugar para ir a empeñar</blockquote>"+selectoMultiple("graficaSH1Select"), "graficaSH1", ""],
["creaContainer","body","SHClase", "Opciones que consideraría", "<blockquote class='bg-success'>De quienes consideran más de un lugar de empeño la primera opción resulta ser el líder de la categoría Monte de Piedad, después se situa Presta Prenda y First Cash</blockquote>"+selectorFilaColumnaCasosA("graficaSH2Select"), "graficaSH2", ""],
["creaContainer","body","SHClase", "Evaluación", "<blockquote class='bg-success'>En general las marcas son muy bien evaluadas por los usuarios de la categoría, ya que las calificaciones se centran en “ Bueno” y “Muy bueno”</blockquote>"+selectorFilaColumnaCasosB("graficaSH3Select"), "graficaSH3", ""],
["creaContainer","body","SHClase", "<b>Asociación de atributos y marcas</b>", "<blockquote class='bg-success'>"+ptext+"</blockquote><h3><small>(Esta gráfica es estática)</small></h3>", "","<img class='img-responsive img-rounded center-block' src='img/mc.png'><hr><img class='img-responsive img-rounded center-block' src='img/mc2.png' style:'height: 600px;'>"],
["creaContainer","body","SHClase", "<b>DQA</b>", "<blockquote class='bg-success'>Para la categoría los 3 atributos más importantes son: <ol><li>“Pago Justo por artículo”, no es asociado a ninguna marca de empeño</li><li>“Contrato claro y Fácil” , Rafael Dónde lo tiene atribuido</li><li>“Buenos horarios”, Presta Prenda tiene la mayor asociación</li></ol></blockquote><h3><small>(Esta gráfica es estática)</small></h3>", "","<img class='img-responsive img-rounded center-block' src='img/dqa.png'>"],
["creaContainer","body","SHClase", "Beneficios emocionales", "<blockquote class='bg-success'>De manera general los participantes de la categoría no están generando un alto vínculo emocional con el consumidor más allá de los beneficios funcionales. Monte de Piedad logra posicionarse por arriba de la media de evaluación</blockquote>"+selectorFilaColumnaCasosB("graficaSH4Select"), "graficaSH4Pre",""],
  ["creaContainer","#graficaSH4Pre","SHClase", "Total de acuerdo + Acuerdo", "", "graficaSH4",""],
  ["creaContainer","#graficaSH4Pre","SHClase", "Totalmente de Acuerdo", "", "graficaSH5",""],

["creaContainer","body","SIClase", "Presta Prenda: Espontáneo", "<blockquote class='bg-success'>Al escuchar el nombre de Presta Prenda lo relacionan  de manera natural: “ Préstamos de dinero” y “ Casa de empeño”, ya en tercera posición con  “Elektra/Casa de empeño de Elektra”</blockquote>", "graficaSI1",""],
//["creaContainer","body","SIClase", "Presta Prenda: Asociación", selectoMultiple("graficaSI2Select"), "graficaSI2",""],
["creaContainer","body","SIClase", "Presta Prenda: Relación con Banco Azteca", "<blockquote class='bg-success'>Los usuarios declaran de forma general, que ambas marcas aportan  de manera positiva a la imagen de Presta Prenda, con un poco mayor de ventaja  BAZ</blockquote>", "graficaSI3",""],
  ["creaContainer","#graficaSI3","SIClase", "<b>Aporte de Imagen</b>", "<p>Banco Azteca</p><blockquote class='bg-success'>La principal aportación positiva de BAZ es el respaldo como institución financiera, aunque también es asociado con “Intereses altos”</blockquote>"+selectoMultiple("graficaSI31Select"), "graficaSI31", ""],
  ["creaContainer","#graficaSI3","SIClase", "<b>Aporte Positivo</b>", "¿Porqué piensa que aporta positivamente? (Aporta muy positivamente + Aporta positivamente)", "graficaSI32", ""],
  ["creaContainer","#graficaSI3","SIClase", "<b>Aporte Negativo</b>", "¿Porqué piensa que afecta negativamente? (Afecta muy negativamente + Afecta negativamente)", "graficaSI33", ""],
["creaContainer","body","SIClase", "Presta Prenda: Relación con Elektra", "", "graficaSI4",""],
  ["creaContainer","#graficaSI4","SIClase", "<b>Aporte de Imagen</b>", "<p>Elektra</p><blockquote class='bg-success'>El ser una “Tienda reconocida” contribuye de manera positiva ,  por  la parte negativa la perciben como una “Tienda muy cara”</blockquote>"+selectoMultiple("graficaSI41Select"), "graficaSI41", ""],
  ["creaContainer","#graficaSI4","SIClase", "<b>Aporte Positivo</b>", "¿Porqué piensa que aporta positivamente? (Aporta muy positivamente + Aporta positivamente)", "graficaSI42", ""],
  ["creaContainer","#graficaSI4","SIClase", "<b>Aporte Negativo</b>", "¿Porqué piensa que afecta negativamente? (Afecta muy negativamente + Afecta negativamente)", "graficaSI43", ""],
["creaContainer","body","SIClase", "Mayor Aporte", selectoMultiple("graficaSI5Select"), "graficaSI5",""],
["creaContainer","body","SIClase", "Ventajas de préstamo prendario", "<blockquote class='bg-success'>La percepción de “No cobrar muchos intereses” y “Pocos requisitos” son las principales ventajas percibidas del target vs un préstamos tradicional bancario</blockquote>", "graficaSI6",""],
["creaContainer","body","SJClase", "", "", "","<img class='img-responsive img-rounded center-block' src='img/contacto2.png'>"]

];
