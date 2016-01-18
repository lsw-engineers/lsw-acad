attla : dialog {
                label = "ATT Cad Specification - [Layer Setup]";
                spacer_1;
                : column {
                          alignment = centered;
						  : row {
                                : boxed_radio_row {
                                                  label = "Drawing Type";
			      						        : radio_button {
				      						                   label = "Record Drawing";
                                                               key = "dt_rd";
                                                               }
      									        : radio_button {
	      			        						           label = "Project Drawing";
                                                               key = "dt_pd";
                                                               }
                                                  }
                                : boxed_radio_row {
                                                  label = "Type of Work";
	      								        : radio_button {
		      								                   label = "Demo";
                                                               key = "tw_d";
                                                               }
						      			        : radio_button {
				        			     			           label = "New";
                                                               key = "tw_n";
                                                               }
                                                  }
                                }
                          : boxed_row       {
                                            label = "50% Equipment Screening";
									        : toggle       {
										                   label = "Mechanical Equipment";
                                                           key = "es_m";
                                                           }
									        : toggle       {
										                   label = "Electrical Equipment";
                                                           key = "es_e";
                                                           }
									        : toggle       {
										                   label = "Telephone Equipment";
                                                           key = "es_t";
                                                           }
                                            }
                         }
                         spacer_1;
                         ok_cancel_err;
                }
