/*************************************************************************

// (c) 2017 - 2019 Wolfram Diestel
// laŭ GPL 2.0

*****************************************************************************/

/** ŝablonoj por flekseble enmeti referencojn, fontojn ktp. **/

/*** XMLŜablono: baza funkcioj de ŝablonoj ***/
var XMLŜablono = function(sxablono) {
    this.sxablono = sxablono;
}

XMLŜablono.prototype = {

    xml: function(args,indent=0) {
        // args: Objekto kiu enhavas la anstataŭigendajn valorojn,  kiuj aperas kiel {...} 
        // en la ŝablono, ekz {rad: "hom", fin: "o",...}
        var sx = this.sxablono.split("\n");
        var resultstr = '', ispaces='';
        for (var i=0; i<sx.length; i++) {
            var line = sx[i];
            var pos = line.indexOf(":");
            if (pos >= 0) {
                var cond = line.slice(0,pos);
                var str = line.slice(pos+1);
            } else {
                cond = '';
                str = line;
            }
            if (this.eval_condition(cond,args)) 
                    resultstr += this.eval_varstring(str,args) + "\n";
            
        }
        if (indent) {
            // var ispaces = new Array(indent+1).join(" ");
            ispaces = ' '.repeat(indent);
            resultstr = ispaces + (resultstr.replace(/\n/g,'\n'+ispaces)).trim();
        }
        return resultstr;   
    },

    eval_condition: function(cond,args) {
        var c = cond.trim();
        return c? new Function("return " + c.replace(/(\w+)/g,"this.$1")).call(args) : true;
    },
                
    eval_varstring: function(str,args) {
        return str.replace(/\{([a-z_]+)\}/g, function(_m,$1){ return args[$1]} );
    }
}

function extend(ChildCls, ParentCls) {
    /*
	ChildCls.prototype = new ParentCls();
    ChildCls.prototype.constructor = ChildCls;
    */
   ChildCls.prototype = Object.create(ParentCls.prototype);
   Object.defineProperty(ChildCls.prototype, 'constructor', { 
        value: ParentCls, 
        enumerable: false, // so that it does not appear in 'for in' loop
        writable: true });
}

/*** XMLArtikolo: krei novan artikolon surbaze de ŝablono ***/

var XMLArtikolo = function(art) {
    XMLŜablono.call(this,xml_sxablonoj.art);
    art.dif = art.dif.replace(/~/g,'<tld/>');
    this.art = art;
}   
extend(XMLArtikolo,XMLŜablono);

XMLArtikolo.prototype.xml = function(indent) {
    return XMLŜablono.prototype.xml.call(this,this.art,indent);
}


/*** XMLDerivaĵo: krei novan derivaĵon surbaze de ŝablono ***/

var XMLDerivaĵo = function(drv) {
    XMLŜablono.call(this,xml_sxablonoj.drv);
    drv.dif = drv.dif.replace(/\n/g,"\n       ").replace(/~/g,'<tld/>');
    drv.mrk = alCx(drv.mrk + '.' + drv.kap.replace(/~/g,'0').replace(/ /g,'_'));
    drv.kap = drv.kap.replace(/~/g,'<tld/>');
    this.drv = drv;
}   
extend(XMLDerivaĵo,XMLŜablono);

XMLDerivaĵo.prototype.xml = function(indent) {
    return XMLŜablono.prototype.xml.call(this,this.drv,indent);
}

/*** XMLSenco: krei novan sencon surbaze de ŝablono ***/

var XMLSenco = function(snc) {
    XMLŜablono.call(this,xml_sxablonoj.snc);
    snc.dif = snc.dif.replace(/\n/g,"\n       ").replace(/~/g,'<tld/>');
    snc.mrk = snc.drvmrk + '.' + snc.mrk;     
    this.snc = snc;
}  
extend(XMLSenco,XMLŜablono);

XMLSenco.prototype.xml = function(indent=2) {
    return XMLŜablono.prototype.xml.call(this,this.snc,indent);
}

/*** XMLFonto, XMLEkzemplo: krei novan fonton resp. ekzemplon surbaze de ŝablono ***/


var XMLFonto = function(fnt) {
    XMLŜablono.call(this,xml_sxablonoj.fnt);
    this.fnt = fnt;
}
extend(XMLFonto,XMLŜablono);

XMLFonto.prototype.xml = function(indent) {
    return XMLŜablono.prototype.xml.call(this,this.fnt,indent);
}


var XMLEkzemplo = function(ekz) {
    XMLŜablono.call(this,xml_sxablonoj.ekz);
    this.fonto = new XMLFonto(ekz);
    ekz.frazo = ekz.frazo.replace(/~/g,'<tld/>');
    this.frazo = ekz.frazo;
}
extend(XMLEkzemplo,XMLŜablono);

XMLEkzemplo.prototype.xml = function(indent) {
    var ekz = {fnt:this.fonto.xml(2).trim(), frazo:this.frazo};
    return XMLŜablono.prototype.xml.call(this,ekz,indent);
}

/*** XMLReferenco, XMLReferencGrupo: krei novan referencon aŭ Referenc-Grupon surbaze de ŝablono ***/

var XMLReferenco = function(ref) {
    XMLŜablono.call(this,xml_sxablonoj.ref);
    if (ref.tip == 'nuda') { ref.tip = '' };
    this.ref = ref;
}
extend(XMLReferenco,XMLŜablono);

XMLReferenco.prototype.xml = function(indent=0) {
    return XMLŜablono.prototype.xml.call(this,this.ref,indent).replace(/\s+$/, '');
}


var XMLReferencGrupo = function(ref) {
    XMLŜablono.call(this,xml_sxablonoj.refgrp);
    if (ref.tip == 'nuda') { ref.tip = '' };
    this.ref = ref;
}
extend(XMLReferencGrupo,XMLŜablono);

XMLReferencGrupo.prototype.xml = function(indent=4) {
    return XMLŜablono.prototype.xml.call(this,this.ref,indent);
}

/*** XMLRimarko: krei novan rimarkon surbaze de ŝablono ***/

var XMLRimarko = function(rim, tip='rim') {
    XMLŜablono.call(this,(tip=='rim'?xml_sxablonoj.rim:xml_sxablonoj.adm));
    this.rim = rim;
}
extend(XMLRimarko,XMLŜablono);

XMLRimarko.prototype.xml = function(indent) {
    return XMLŜablono.prototype.xml.call(this,this.rim,indent);
}

/*** XMLBildo: krei novan bildo-strukturon surbaze de ŝablono ***/

var XMLBildo = function(bld) {
    XMLŜablono.call(this,xml_sxablonoj.bld);

    // https://upload.wikimedia.org/wikipedia/commons/6/61/Glaciar_del_Aneto_%283.404m%29.jpg
    // -> https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/Glaciar_del_Aneto_%283.404m%29.jpg/640px-Glaciar_del_Aneto_%283.404m%29.jpg
    if (bld.url.startsWith('https://upload.wikimedia.org/wikipedia/commons/')) {
        var prefix_len = 'https://upload.wikimedia.org/wikipedia/commons/'.length;
        var parts = bld.url.split('/');
        var imgname = parts[parts.length -1];
        if (bld.url.endsWith('.svg')) {
            bld.url = '&WCU;/' + bld.url.substr(prefix_len);
        } else {
            bld.url = '&WCU;/thumb/' + bld.url.substr(prefix_len) + '/' + bld.lrg + 'px-' + imgname;
            bld.lrg = ''
        }
    }
    bld.fnt = bld.fnt.replace("https://commons.wikimedia.org/wiki","&WCW;"); 
    bld.frazo = bld.frazo.replace(/~/g,'<tld/>');
    bld.prm = bld.prm.replace(/public domain/i,'PD');
    this.bld = bld;
}
extend(XMLBildo,XMLŜablono);

XMLBildo.prototype.xml = function(indent) {
    return XMLŜablono.prototype.xml.call(this,this.bld,indent);
}


/*** HTMLFonto: krei novan fonton (ekz. en trov-listo) surbaze de ŝablono ***/

var HTMLFonto = function(bib_src) {
    this.source = bib_src;
    this.v = new XMLŜablono(html_sxablonoj.vrk);
    this.b = new XMLŜablono(html_sxablonoj.bib);
    this.vt = new XMLŜablono(html_sxablonoj.vrk_title);
    this.bt = new XMLŜablono(html_sxablonoj.bib_title);
}

HTMLFonto.prototype = {
    bib_text: function(bib) {
        for (var i=0;i<this.source.length;i++) {
            entry = this.source[i];
            if (entry.value == bib) {
                return entry.label
            }
        }
    },
    
    html: function(fnt) {
        var _html = '', f = {};
        var bib = fnt.bib;

        var f = {
           vrk: this.v.xml(fnt).trim(),
           bib: this.b.xml(fnt).trim(),
           aut: fnt.aut,
           lok: fnt.lok
        }
    
        f.vrk = this.vt.xml(f).trim();
        if (bib) {
            f.bib_text =  this.bib_text(bib);
            _html = this.bt.xml(f).trim();
        } else {
            _html = f.vrk;
        }

        return _html;
    }
}

/*** HTMLTrovoDt, HTMLTrovoDdBld: krei novan trovon (ekz. en trov-listo) surbaze de ŝablono ***/

var HTMLTrovoDt = function() {
    this.t = new XMLŜablono(html_sxablonoj.dt_trovo);
    this.tc = new XMLŜablono(html_sxablonoj.dt_trovo_cit);
}

HTMLTrovoDt.prototype = {
    html: function(trv) {
        if (trv.title.includes(trv.url)) {
            return this.tc.xml(trv).trim();
        } else {
            return this.t.xml(trv).trim();
        }
    }
}

var HTMLTrovoDdBld = function() {
    this.dd = new XMLŜablono(html_sxablonoj.dd_trovo_bld);
}

HTMLTrovoDdBld.prototype = {
    html: function(res) {
        return this.dd.xml(res).trim();
    }
}

/* listeroj de eraro- kaj avertolistoj */
var HTMLError = function() {
    this.li = new XMLŜablono(html_sxablonoj.err_msg);
}

HTMLError.prototype = {
    html: function(err) {
        return this.li.xml(err).trim();
    }
}


/** ŝablonoj elekteblaj kaj prilaboreblaj per menuo Ŝablonoj **/

var SncŜablono = function(sxbl) {
    this.sxablono = snc_sxablonoj[sxbl];
}

SncŜablono.prototype = {
    form: function() {
        return quoteattr(this.sxablono).replace(/\{([osre]):([^\}]+)\}/g,this.form_element);
    },

    html: function() {
        var lines = this.form().split('\n');
        var html = '';
        for (let i=0; i<lines.length; i++) {
            html += '<tr><td><b class="sxbl">&ndash;</b></td><td><pre class="line">'+lines[i]+'</pre></td></tr>';
        }
        return html;
    },
    
    form_element: function(match,tipo,teksto,offset) {
      var id = tipo + '_' + offset;
      var iid = 'i' + id;
      switch (tipo) {
          //case 'o': 
          //      return '<span class="sxbl" id="' + span_id + '">'+teksto+'</span><input type="checkbox" checked/>'; 
          //      break;
          case 's':
              var text_len = teksto.length;
              return '<span class="sxbl" id="' + id + '"><input type="text" size="' + 
                text_len + '" id="' + iid + '" value="' + teksto + '" /></span></input></span>';
              break;
          case 'r':
              return '<span class="sxbl" id="' + id + '">' + teksto + 
                '</span><button class="ui-button ui-corner-all ui-button-icon-only">' +
                '<span class="ui-icon ui-icon-search"></button>';
              break;
          case 'e':
              return '<span class="sxbl" id="' + id + '">' + teksto + 
                '</span><button class="ui-button ui-corner-all ui-button-icon-only">' +
                '<span class="ui-icon ui-icon-search"></button>';
              break;
      }    
    }
}

