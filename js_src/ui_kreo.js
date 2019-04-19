/**
 * @license (c) 2016 - 2018 Wolfram Diestel
 * lau GPL 2.0
 */

import menu_init from './ui_menu.js';
import dlg_init from './ui_dlg.js';
import tabl_init from './ui_tabl.js';

//*********************************************************************************************
//* Preparoj 
//*********************************************************************************************

var preflng='en';

$(document).ready(function () {
       
    // prepari la redaktilo-aranĝon
    $(function () {
      
      tabl_init()
      menu_init();
      dlg_init();

      // mus-notoj
      $( document ).tooltip();
    
    });
});


