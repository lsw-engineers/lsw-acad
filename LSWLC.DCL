lswlc : dialog {
		label = "LSW Layer Creator";
		            : image {
		               aspect_ratio = 0;
		               width = 75 ;
		               key = "Logo_box" ;
		               height = 5 ;
		               fixed_width = true ;
		               fixed_height = true ;
		            }
		:row {
		:boxed_radio_column {
			label = "Discipline";
			key = "DISC";
			alignment = centered;
			fixed_width = true;
			width = 15;
			: radio_button 	{
					label = "Electrical";
					key = "ELEC";
					mnemonic = "E";
					value = "0";
					}
			: radio_button 	{
					label = "Mechanical";
					key = "MECH";
					mnemonic = "M";
					value = "0";
					}
			: radio_button 	{
					label = "Plumbing";
					key = "PLUM";
					mnemonic = "P";
					value = "0";
					}
		}
		: boxed_column {
			label = "Drawing Type" ;
			key = "Dwg_Type";
			fixed_width = true;
			width = 53;
			: list_box {
				key = "Dwg_Type_List" ;
				multiple_select = true;
				is_enabled = false;
				fixed_height = true;
				height = 9;
			}
		}
		}
		:row {
		:boxed_column {
			label = "Work Type";
			alignment = centered;
			fixed_width = true;
			width = 48;

			: toggle 	{
					label = "New (-NEW Layers)";
					key = "NEW";
					mnemonic = "N";
					value = "1";
					}
			: toggle 	{
					label = "Existing (-EXST Layers)";
					key = "EXST";
					mnemonic = "x";
					value = "0";
					}
			: toggle 	{
					label = "Demolition (-DEMO Layers)";
					key = "DEMO";
					mnemonic = "D";
					value = "0";
					} 
		}
		:boxed_radio_column {
			label = "Overwrite Layers?";
			key = "Overwrite";
			fixed_width = true;
			width = 20;
			: radio_button {
				label = "Yes";
				key = "Overwrite_yes";
			}
			: radio_button {
				label = "No";
				key = "Overwrite_no";
				value = 1;
			}
		}
		} 
                ok_cancel_help;
                }
