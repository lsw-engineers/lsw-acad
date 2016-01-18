dcl_settings : default_dcl_settings { audit_level = 3 ; }
ATTLAY : dialog {
   label = "AT&T Layer Setup Tool" ;
   : row {
      : boxed_radio_column {
         label = "Discipline" ;
         : radio_button {
            key = "Disc_ARCH" ;
            label = "&Architectural" ;
         }
         : radio_button {
            key = "Disc_BSYS" ;
            label = "&Building Systems" ;
         }
         : radio_button {
            key = "Disc_CIVL" ;
            label = "&Civil/Site" ;
         }
         : radio_button {
            key = "Disc_CTRL" ;
            label = "Controls (&K)" ;
         }
         : radio_button {
            key = "Disc_DVLP" ;
            label = "&Developement" ;
         }
         : radio_button {
            key = "Disc_ELEC" ;
            label = "&Electrical" ;
         }
         : radio_button {
            key = "Disc_FIRE" ;
            label = "&Fire Safety" ;
         }
         : radio_button {
            key = "Disc_MECH" ;
            label = "&Mechancial" ;
         }
         : radio_button {
            key = "Disc_PLUM" ;
            label = "&Plumbing" ;
         }
         : radio_button {
            key = "Disc_STRU" ;
            label = "&Structural" ;
         }
         : radio_button {
            key = "Disc_TELE" ;
            label = "&Tele Equipment" ;
         }
      }
      : column {
         : boxed_column {
            label = "Drawing Type" ;
            : list_box {
               key = "Draw_Type" ;
               tabs = "4" ;
               width = 30 ;
            }
         }
         : boxed_column {
            label = "Reset What?" ;
            : toggle {
               key = "Reset_Color" ;
               label = "Default Layer Colors" ;
            }
            : toggle {
               key = "Reset_LT" ;
               label = "Default Layer Linetypes" ;
            }
         }
         : boxed_column {
            label = "Options" ;
            : toggle {
               key = "XREF_Lays" ;
               label = "Include XREF'd Layers" ;
            }
         }
      }
      : column {
         : boxed_row {
            label = "ATT CAD Tool" ;
            : image_button {
               width = 18 ;
               key = "ATT_CAD_Tool" ;
               height = 10 ;
               fixed_width = true ;
               fixed_height = true ;
               color = black ;
               allow_accept = true ;
            }
         }
         : boxed_row {
            label = "For What Layers?" ;
            : radio_column {
               alignment = top ;
               children_fixed_width = true ;
               fixed_width = true ;
               width = 2 ;
               : radio_button {
                  fixed_width = true ;
                  key = "For_All_Lays" ;
                  width = 2 ;
               }
               spacer_1 ;
               : radio_button {
                  fixed_width = true ;
                  key = "For_Tog_Lays" ;
                  width = 2 ;
               }
               : spacer {
                  height = 5 ;
               }
            }
            : column {
               : text {
                  label = "All Layers" ;
               }
               spacer_1 ;
               : toggle {
                  key = "For_NEWW" ;
                  label = "-NEWW's" ;
               }
               : toggle {
                  key = "For_EXST" ;
                  label = "-EXST's" ;
               }
               : toggle {
                  key = "For_DEMO" ;
                  label = "-DEMO's" ;
               }
               : toggle {
                  key = "For_FUTR" ;
                  label = "-FUTR" ;
               }
               : toggle {
                  key = "For_NilSuffix" ;
                  label = "Nil Suffixes" ;
               }
            }
         }
      }
   }
   : boxed_radio_row {
      label = "Do What?" ;
      : radio_button {
         key = "Do_Create" ;
         label = "CREATE Layers for Selected Drawing Type." ;
      }
      : radio_button {
         key = "Do_Reset" ;
         label = "RESET Existing Layers" ;
      }
   }
   errtile ;
   ok_cancel_help ;
}
