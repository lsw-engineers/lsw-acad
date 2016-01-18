// att_fit.dcl

// dde9s command dialog box definition


dde9s : dialog {
                  : row {
                        : column {
                                 : boxed_radio_column {
                                                      label = "Type :";
                                                      : radio_button {
                                                                     label = "90 Long Radius";
                                                                     key = "el90lr";
                                                                     }
                                                      : radio_button {
                                                                     label = "90 Short Radius";
                                                                     key = "el90sr";
                                                                     }
                                                      : radio_button {
                                                                     label = "45 Long Radius";
                                                                     key = "el45lr";
                                                                     }
                                                      : radio_button {
                                                                     label = "45 Short Radius";
                                                                     key = "el45sr";
                                                                     }
                                                      }
                                 : boxed_radio_column {
                                                      label = "Connector :";
                                                      : radio_button {
                                                                     label = "Flanged";
                                                                     key = "dif_els";
                                                                     }
                                                      : radio_button {
                                                                     label = "Butt Weld";
                                                                     key = "wdf_els";
                                                                     }
                                                      : radio_button {
                                                                     label = "Split Coupling";
                                                                     key = "scf_els";
                                                                     }
                                                      }
                                 }
                                 : boxed_column {
                                                label = "View :" ;
                                                alignment = centered ;
                                                : image_button {
                                                                //aspect_ratio = 1.0;
                                                                width = 6;
                                                                color = 0;
                                                                key = "s_cl";
                                                               }
                                                : image_button {
                                                                //aspect_ratio = 1.0;
                                                                width = 6;
                                                                color = 0;
                                                                key = "s_cp";
                                                               }
                                                : image_button {
                                                                //aspect_ratio = 1.0;
                                                                width = 6;
                                                                color = 0;
                                                                key = "d_cl";
                                                               }
                                                : image_button {
                                                                //aspect_ratio = 1.0;
                                                                width = 6;
                                                                color = 0;
                                                                key = "d_cp";
                                                               }
                                                : image_button {
                                                                //aspect_ratio = 1.0;
                                                                width = 6;
                                                                color = 0;
                                                                key = "u_cl";
                                                               }
                                                : image_button {
                                                                //aspect_ratio = 1.0;
                                                                width = 6;
                                                                color = 0;
                                                                key = "u_cln";
                                                               }
                                                : image_button {
                                                                //aspect_ratio = 1.0;
                                                                width = 6;
                                                                color = 0;
                                                                key = "u_cp";
                                                               }
                                                } // end of boxed_column
                                 : boxed_column {
                                                 label = "Size";
                                                : list_box {
                                                            key = "f_sz";
                                                           } // end of list box
                                                } // end of boxed_column

                        } // end of row
                        ok_cancel_help_errtile ;
                  } // end of dde9s dialog


// ddtee dialog box definition

ddtee : dialog {
                : row {
                      :column {
                      : boxed_radio_column {
                                            label = "Type :";
                                            : radio_button {
                                                           label = "Flanged";
                                                           key = "dif_tee";
                                                           }
                                            : radio_button {
                                                           label = "Butt Weld";
                                                           key = "wdf_tee";                                                            }
                                            : radio_button {
                                                           label = "Split Coupling";
                                                           key = "scf_tee";
                                                           }
                                            } // end of boxed_radio_column      
                                     : image_button {
                                                    alignment = centered;
                                                    width = 7;
                                                    fixed_width = true;
                                                    aspect_ratio = 1.0;
                                                    color = 0;
                                                    key = "s_cl";
                                                    }
                                     : image_button {
                                                    alignment = centered;
                                                    width = 7;
                                                    fixed_width = true;
                                                    aspect_ratio = 1.0;
                                                    color = 0;
                                                    key = "d_cl";
                                                    }
                                     : image_button {
                                                    alignment = centered;
                                                    width = 7;
                                                    fixed_width = true;
                                                    aspect_ratio = 1.0;
                                                    color = 0;
                                                    key = "u_cl";
                                                    }
                                     } // end boxed_column
                      : boxed_column {
                                     label = "Sizes ";
                                     : list_box {
                                                width = 9;
                                                key = "f_sz";
                                                }
                                     } // end boxed_column
                      : boxed_column {
                                     label = "Dimensions <inches>";
                                     : edit_box {
                                                label = "Run";
                                                edit_width = 5;
                                                key = "r_1";
                                                }
                                     : edit_box {
                                                label = "Branch";
                                                edit_width = 5;
                                                key = "l_2";
                                                }
                                     : edit_box {
                                                label = "Run Dia";
                                                edit_width = 5;
                                                key = "pd1";
                                                }
                                     : edit_box {
                                                label = "Branch Dia";
                                                edit_width = 5;
                                                key = "pd2";
                                                }
                                     : edit_box {
                                                label = "Run Cplng Dia";
                                                edit_width = 5;
                                                key = "cd1";
                                                }
                                     : edit_box {
                                                label = "Run Cplng Width";
                                                edit_width = 5;
                                                key = "cw1";
                                                }
                                     : edit_box {
                                                label = "Branch Cplng Dia";
                                                edit_width = 5;
                                                key = "cd2";
                                                }
                                     : edit_box {
                                                label = "Branch Cplng Width";
                                                edit_width = 5;
                                                key = "cw2";
                                                }
                                     } // end boxed_column
                      } // end row
                      ok_cancel_help_errtile ;
                  } // end of ddtee dialog


ddcplg : dialog {
                label = Couplings;
                : row {
                      :column {    
                                     : image_button {
                                                    alignment = centered;
                                                    width = 7;
                                                    fixed_width = true;
                                                    aspect_ratio = 1.0;
                                                    color = 0;
                                                    key = "dcp";
                                                    }
                                     : image_button {
                                                    alignment = centered;
                                                    width = 7;
                                                    fixed_width = true;
                                                    aspect_ratio = 1.0;
                                                    color = 0;
                                                    key = "scp";
                                                    }
                                     : image_button {
                                                    alignment = centered;
                                                    width = 7;
                                                    fixed_width = true;
                                                    aspect_ratio = 1.0;
                                                    color = 0;
                                                    key = "wcp";
                                                    }
                                     } // end boxed_column
                      : boxed_column {
                                     label = "Sizes ";
                                     : list_box {
                                                width = 9;
                                                key = "f_sz";
                                                }
                                     } // end boxed_column
                      : boxed_column {
                                     label = "Dimensions <inches>";
                                     : edit_box {
                                                label = "Length";
                                                edit_width = 5;
                                                key = "r1";
                                                }                                     
                                     : edit_box {
                                                label = "Pipe Dia";
                                                edit_width = 5;
                                                key = "pd1";
                                                }
                                     : edit_box {
                                                label = "Couplng Dia";
                                                edit_width = 5;
                                                key = "cd1";
                                                }
                                     : edit_box {
                                                label = "Couplng Width";
                                                edit_width = 5;
                                                key = "cw1";
                                                }
                                     } // end boxed_column
                      } // end row
                      ok_cancel_help_errtile ;
                  } // end of ddcplg dialog      


// HVAC Dilaog Definitions



//Diffusers definition


ddiff : dialog {
                label = "Diffusers";
                spacer_1;
                : column {
                          alignment = centered;
                          : edit_box {
                                      label = "Size <inches>";
                                      edit_width = 7;
                                      key = "d_sz";
                                     }
                         }
                         spacer_1;
                         : boxed_column {
                                        label = "   PLAIN      SUPPLY     EXTRACT";
                                        : row {
                                              : image_button {
                                                              key = "sqr_p";
                                                              height = 5;
                                                              aspect_ratio = 0.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "sqr_s";
                                                              height = 5;
                                                              aspect_ratio = 0.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "sqr_e";
                                                              height = 5;
                                                              aspect_ratio = 0.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                              }
                                              }
                                        : row {
                                              : image_button {
                                                              key = "rnd_p";
                                                              height = 5;
                                                              aspect_ratio = 0.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "rnd_s";
                                                              height = 5;
                                                              aspect_ratio = 0.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "rnd_e";
                                                              height = 5;
                                                              aspect_ratio = 0.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              }
                                        : row {
                                              : image_button {
                                                              key = "sid_p";
                                                              height = 5;
                                                              aspect_ratio = 0.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "sid_s";
                                                              height = 5;
                                                              aspect_ratio = 0.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "sid_e";
                                                              height = 5;
                                                              aspect_ratio = 0.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              }
                                        }
                                        spacer_1;
                                        ok_cancel_err;
                }

//Transformations

ddtrf : dialog {
                label = "HVAC Transformation Fittings";
                spacer_1;
                : column {
                         alignment = centered;
                         : edit_box {
                                    label = "Main <inches>";
                                    edit_width = 7;
                                    key = "m_d";
                                    }
                         : edit_box {
                                    label = "Branch <inches>";
                                    edit_width = 7;
                                    key = "b_d";
                                    }
                         }
                         spacer_1;
                         : boxed_column {
                                        label = "          SQUARE TO ROUND";
                                        : row {
                                              : image_button {
                                                              key = "sq_r_c";
                                                              width = 5;
                                                              aspect_ratio = 1.75;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "sq_r_el";
                                                              width = 5;
                                                              aspect_ratio = 1.75;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "sq_r_er";
                                                              width = 5;
                                                              aspect_ratio = 1.75;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              }
                                        }
                                        spacer_1;
                         : boxed_column {
                                        label = "          ROUND TO SQUARE";
                                        : row {
                                              : image_button {
                                                              key = "r_sq_c";
                                                              width = 5;
                                                              aspect_ratio = 1.75;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "r_sq_el";
                                                              width = 5;
                                                              aspect_ratio = 1.75;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "r_sq_er";
                                                              width = 5;
                                                              aspect_ratio = 1.75;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              }
                                        }
                                        spacer_1;
                                        ok_cancel_err;
                }


// Round duct

ddrndd : dialog {
                label = "HVAC Duct Fittings (Round)";
                spacer_1;
                : column {
                         alignment = centered;
                         : edit_box {
                                     label = "Main Diameter <inches>";
                                     edit_width = 10;
                                     key = "m_d";
                                    }
                         : edit_box {
                                     label = "Branch Diameter <inches>";
                                     edit_width = 10;
                                     key = "b_d";
                                    }
                         :  text {
                                  key = d_des;
                                 } 
                         }
                         : boxed_column {
                                        : row {
                                              : image_button {
                                                              key = "s9_rs";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "u9_rs";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "d9_rs";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "s4_rs";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "s3_rs";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              }
                                        : row {
                                              : image_button {
                                                              key = "s9_rl";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "u9_rl";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "d9_rl";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "s4_rl";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "s3_rl";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              }
                                        : row {
                                              : image_button {
                                                              key = "tw_rs";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "tee_s";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "tee_a";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "tee_u";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "tee_d";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              }
                                        : row {
                                              : image_button {
                                                              key = "lat_l";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "lat_r";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "red_c";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "red_el";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              : image_button {
                                                              key = "red_er";
                                                              width = 5;
                                                              aspect_ratio = 1.0;
                                                              color = 0;
                                                              allow_accept = true;
                                                             }
                                              }
                                        }
                                        spacer_1;
                                        ok_cancel_err;
                }

// general blocks and lisp programs dialog

ddgen : dialog {
                label = "General Blocks";
                spacer_1;
                :row {
                     : column {
                               : image_button {
                                               key = "gen1";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               : image_button {
                                               key = "gen2";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               : image_button {
                                               key = "gen3";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               : image_button {
                                               key = "gen4";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               }
                     : column {
                               : image_button {
                                               key = "gen5";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               : image_button {
                                               key = "gen6";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               : image_button {
                                               key = "gen7";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               : image_button {
                                               key = "gen8";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               }
                     : column {
                               : image_button {
                                               key = "gen9";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               : image_button {
                                               key = "gen10";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               : image_button {
                                               key = "gen11";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               : image_button {
                                               key = "gen12";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                               }
                     : column {
                               spacer_0;
                               : image_button {
                                               key = "gen13";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                                              spacer_0;
                               : image_button {
                                               key = "gen14";
                                               width = 7;
                                               aspect_ratio = 1.0;
                                               color = 0;
                                               allow_accept = true;
                                              }
                                              spacer_0;
                               }
                     }
                     spacer_1;
                     ok_cancel_err;
               }
