`ifdef BEHAV_CHANN_EXT_SV
module behav_chann
   import types_pkg::*;
  (svo, level, delay, dcg, phcal_en, dcshift_clr, dcshift_out, tas_r);

   timeunit         1ns;
   timeprecision    1ps;

   input    model   level;
   input            phcal_en;

   input            dcshift_clr;
   output   real    dcshift_out;
 
   output   sreal   svo;
   output   real    delay; //channel delay in ns
   output           dcg; //dc gain real
   input    int     tas_r;

   initial initialize_sreal(svo);
   
   //  Channel            /////////////
   //  c0/(s+p0)[c1/(s+p1) + c2/(s+p2)]
   //  =[c0c1/(p2-p0) + c0c2/(p3-p0)]*1/(s+p0) + [-c0c1/(p2-p0)]*1/(s+p2) + [-c0c2/(p3-p0)]*1/(s+p3)

   real             CH_DELAY;
   real             CH_DCG;
   string           CH_POLES;
   string           CH_COEFF;

   //Coeff/pole scaling
   //real       c0n1_scale = 5.0/6*6/9; //1.0; //Change 12/12/18:  9/10 x 5/6 = 3/4
   //real       w0n1_scale = 1.33;
   //real       c2_scale   = 5.0/6*6/9; //1.0; //Change 12/12/18:  To bring VREF within range 
   //real       w2_scale   = 1.33;
   // tatic channel: Coefficients are static.

   complex c[$];
   complex wp[$];
   //Coeff/pole scaling
   real 	    c0n1_scale = 5.0/6*6/9; //1.0; //Change 12/12/18:  9/10 x 5/6 = 3/4
   real 	    w0n1_scale = 1.33;
   real 	    c2_scale   = 5.0/6*6/9; //1.0; //Change 12/12/18:  To bring VREF within range 
   real 	    w2_scale   = 1.33;
   real             dcg_prev   = 1.0;
   real dcg = 1.0;

   //scal phos pole
   //TODO: Currently using tas_r to differential between 106G and 53G scal phos pole.
   //      Only 106G and 53G support scal phos in BB
   //      53G dcg is fixed to 2 (spec from analog team)
   real phos_pole = 41.47e9; //6.6G * 2 * pi

   always @(level or phcal_en or tas_r)
     begin
     if (phcal_en===1'b0) begin
	case(level)
	  
	  SIMPLE : begin //1 Pole simple channel model
             //c  = {}; c  = '{ '{0.4*51.9375e9, 0.0} };
             //wp = {}; wp = '{ '{0.4*125e9, 0.0} } ;
	     c = {}; c = '{
			   '{-c0n1_scale*1.7680e+10,-c0n1_scale*3.4384e+09},
			   '{-c0n1_scale*1.7680e+10, c0n1_scale*3.4384e+09},
			   '{   c2_scale*4.5636e+10, 0.0000e+00}
			   };
             wp={}; wp = '{
			   '{ w0n1_scale*4.7321e+10,-w0n1_scale*8.7968e+10},
			   '{ w0n1_scale*4.7321e+10, w0n1_scale*8.7968e+10},
			   '{   w2_scale*4.1580e+10,-0.0000e+00}
			   };
             delay      = 0.1234;  //Fake delay
             dcg        = 0.4137;
	  end
	  
	  MEDIUM : begin //12 Poles channel models
             //----------------------------------------------------------------------------------
             // Final channel model release - Refer to the readme file in the channel folder
             //----------------------------------------------------------------------------------
             if ($value$plusargs ("CH_DELAY=%g" , CH_DELAY))	delay = CH_DELAY;
             else 	                                        delay = 2.691;                   // Default channel is CHID79
             if ($value$plusargs ("CH_DCG=%g" , CH_DCG))	dcg = CH_DCG;
             else 	                                        dcg = 0.8564;                   // Default channel is CHID79

             if ($value$plusargs ("CH_POLES=%s" , CH_POLES)) 
	       begin
		  wp = {}; cmdstr2complex(wp, ",", ",", CH_POLES);
               end
             else  // Default is CHID79
	       begin
		  wp = {}; wp = '{
                                '{2.050828e+09,-2.080943e+11},
                                '{2.050828e+09,2.080943e+11},
                                '{1.932650e+10,-0.000000e+00},
                                '{1.334484e+10,-2.000741e+11},
                                '{1.334484e+10,2.000741e+11},
                                '{4.881326e+10,-1.557341e+11},
                                '{4.881326e+10,1.557341e+11},
                                '{5.135905e+10,-6.666082e+10},
                                '{5.135905e+10,6.666082e+10},
                                '{4.391974e+09,-0.000000e+00},
                                '{7.805591e+08,-0.000000e+00},
                                '{1.065418e+08,-0.000000e+00}
				};
               end
             if ($value$plusargs ("CH_COEFF=%s" , CH_COEFF)) 
	       begin
		  c = {}; cmdstr2complex(c, ",", ",", CH_COEFF);
               end
             else  // Default is CHID79
	       begin
		  c= {};    c = '{
                                '{1.577110e+06,2.193011e+06},
                                '{1.577110e+06,-2.193011e+06},
                                '{1.541383e+10,0.000000e+00},
                                '{-3.703664e+07,-3.388958e+07},
                                '{-3.703664e+07,3.388958e+07},
                                '{6.078084e+08,-1.452061e+09},
                                '{6.078084e+08,1.452061e+09},
                                '{-8.544139e+09,9.067804e+09},
                                '{-8.544139e+09,-9.067804e+09},
                                '{1.080920e+09,0.000000e+00},
                                '{6.090155e+07,0.000000e+00},
                                '{1.051927e+06,0.000000e+00}
				};
	       end
	  end // case: MEDIUM

	  FULL : 
	    begin //48 Poles channel models
             //----------------------------------------------------------------------------------
             // Final channel model release - Refer to the readme file in the channel folder
             //----------------------------------------------------------------------------------
             if ($value$plusargs ("CH_DELAY=%g" , CH_DELAY))	delay = CH_DELAY;
             else 	                                        delay = 2.432;                  // Default channel is CHID79
             if ($value$plusargs ("CH_DCG=%g" , CH_DCG))	dcg = CH_DCG;
             else 	                                        dcg = 0.8544;                   // Default channel is CHID79

             if ($value$plusargs ("CH_POLES=%s" , CH_POLES)) 
	       begin
		  wp = {}; cmdstr2complex(wp, ",", ",", CH_POLES);
               end
             else  // Default is CHID79
	       begin
		  wp={};    wp= '{
                    '{2.686986e+09,-2.456920e+11}, '{2.686986e+09,2.456920e+11},  '{9.743610e+09,-2.242163e+11}, '{9.743610e+09,2.242163e+11},
                    '{3.590372e+09,-2.149795e+11}, '{3.590372e+09,2.149795e+11},  '{1.450926e+09,-2.119428e+11}, '{1.450926e+09,2.119428e+11},
                    '{2.407570e+09,-2.051018e+11}, '{2.407570e+09,2.051018e+11},  '{1.272534e+09,-1.943949e+11}, '{1.272534e+09,1.943949e+11},
                    '{4.521611e+09,-1.881304e+11}, '{4.521611e+09,1.881304e+11},  '{7.067190e+09,-1.690892e+11}, '{7.067190e+09,1.690892e+11},
                    '{9.505817e+09,-1.525277e+11}, '{9.505817e+09,1.525277e+11},  '{7.831464e+09,-1.336330e+11}, '{7.831464e+09,1.336330e+11},
                    '{8.183658e+09,-1.117853e+11}, '{8.183658e+09,1.117853e+11},  '{1.033644e+10,-9.218244e+10}, '{1.033644e+10,9.218244e+10},
                    '{1.201964e+10,-7.424030e+10}, '{1.201964e+10,7.424030e+10},  '{1.297479e+10,-5.580534e+10}, '{1.297479e+10,5.580534e+10},
                    '{1.455090e+10,-3.701510e+10}, '{1.455090e+10,3.701510e+10},  '{1.607670e+10,-2.051027e+10}, '{1.607670e+10,2.051027e+10},
                    '{2.291367e+08,-2.723005e+10}, '{2.291367e+08,2.723005e+10},  '{2.495430e+08,-2.479488e+10}, '{2.495430e+08,2.479488e+10},
                    '{1.490843e+10,-3.523125e+09}, '{1.490843e+10,3.523125e+09},  '{3.371085e+09,-0.000000e+00}, '{1.000972e+08,-9.286627e+09},
                    '{1.000972e+08,9.286627e+09},  '{3.331586e+08,-2.988132e+09}, '{3.331586e+08,2.988132e+09},  '{6.251046e+08,-0.000000e+00},
                    '{6.197990e+07,-1.438317e+08}, '{6.197990e+07,1.438317e+08},  '{4.894851e+05,-3.634552e+07}, '{4.894851e+05,3.634552e+07}
				};
               end
             if ($value$plusargs ("CH_COEFF=%s" , CH_COEFF)) 
	       begin
		  c = {}; cmdstr2complex(c, ",", ",", CH_COEFF);
               end
             else  // Default is CHID79
	       begin
		  c = {};   c = '{
                    '{-1.447765e+05,1.558187e+06},  '{-1.447765e+05,-1.558187e+06}, '{2.490844e+07,2.445170e+07},   '{2.490844e+07,-2.445170e+07},
                    '{1.685015e+07,-9.481413e+06},  '{1.685015e+07,9.481413e+06},   '{1.355888e+06,-4.522408e+06},  '{1.355888e+06,4.522408e+06},
                    '{1.062701e+07,5.735290e+06},   '{1.062701e+07,-5.735290e+06},  '{5.685041e+06,-3.616267e+06},  '{5.685041e+06,3.616267e+06},
                    '{8.410344e+07,1.006673e+07},   '{8.410344e+07,-1.006673e+07},  '{3.283525e+08,-1.324770e+08},  '{3.283525e+08,1.324770e+08},
                    '{2.550171e+07,-9.067827e+08},  '{2.550171e+07,9.067827e+08},   '{-7.063504e+08,-8.620165e+08}, '{-7.063504e+08,8.620165e+08},
                    '{-1.593263e+09,-1.469923e+09}, '{-1.593263e+09,1.469923e+09},  '{-5.972989e+09,-9.107848e+08}, '{-5.972989e+09,9.107848e+08},
                    '{-9.641958e+09,9.785557e+09},  '{-9.641958e+09,-9.785557e+09}, '{3.245430e+09,2.613586e+10},   '{3.245430e+09,-2.613586e+10},
                    '{5.207967e+10,3.004525e+10},   '{5.207967e+10,-3.004525e+10},  '{8.596367e+10,-9.786363e+10},  '{8.596367e+10,9.786363e+10},
                    '{-1.571798e+05,1.123667e+06},  '{-1.571798e+05,-1.123667e+06}, '{-7.191487e+05,1.163186e+06},  '{-7.191487e+05,-1.163186e+06},
                    '{-1.246478e+11,-2.574225e+11}, '{-1.246478e+11,2.574225e+11},  '{1.521442e+09,0.000000e+00},   '{1.394183e+05,4.036005e+05},
                    '{1.394183e+05,-4.036005e+05},  '{5.888078e+04,-6.463933e+05},  '{5.888078e+04,6.463933e+05},   '{5.084407e+07,0.000000e+00},
                    '{-9.835258e+04,-1.990336e+05}, '{-9.835258e+04,1.990336e+05},  '{-1.783004e+03,-1.310408e+04}, '{-1.783004e+03,1.310408e+04}
				};
	       end
	    end

      default :
	begin
	     c = {}; c = '{
			   '{-c0n1_scale*1.7680e+10,-c0n1_scale*3.4384e+09},
			   '{-c0n1_scale*1.7680e+10, c0n1_scale*3.4384e+09},
			   '{   c2_scale*4.5636e+10, 0.0000e+00}
			   };
             wp={}; wp = '{
			   '{ w0n1_scale*4.7321e+10,-w0n1_scale*8.7968e+10},
			   '{ w0n1_scale*4.7321e+10, w0n1_scale*8.7968e+10},
			   '{   w2_scale*4.1580e+10,-0.0000e+00}
			   };
           //c  = {}; c  = '{ '{51.9375e9, 0.0} };
           //wp = {}; wp = '{ '{125e9, 0.0} } ;
           delay      = 0.1234; //Fake delay
           dcg        = 0.4137;
        end
	endcase // case (level)

        end else if (phcal_en===1'b1) begin //phcal_en  LPF Wc = 6.6e9*2*pi, atten modelled inside rx_term_core
            case(level)
                SIMPLE       : phos_pole = (tas_r === 5) ? 31.47e9 : 41.47e9;
                MEDIUM, FULL : phos_pole = 41.47e9;
                default      : phos_pole = (tas_r === 5) ? 31.47e9 : 41.47e9;
            endcase

           delay = 0.1234; //Fake delay 
           dcg   = 1.0;
           foreach(c[i]) begin
                if (i==0) begin c[i] = '{phos_pole, 0.0}; wp[i] = '{phos_pole, 0.0}; end
                else      begin c[i] = '{1e-28,0.0};    wp[i] = wp[i];           end
           end
        end else begin
            c    = {};
           wp    = {};
           delay = 0;
           dcg   = 1.0;
        end

        //foreach (svo.c[i]) begin svo.c[i] = '{realX,realX}; svo.w0[i] = '{realX,realX}; end //Reset

        foreach (c[i]) begin
            svo.c[i]         = c[i];
            svo.w0[i]        = wp[i];
        end

        svo.t         = $realtime * 1e-9;
        svo.trig      = ~svo.trig;
     end // always_comb begin
   
   always @(dcg or posedge dcshift_clr) begin
        if (dcshift_clr === 1'b0)  dcshift_out = dcg/dcg_prev;
        else                       dcshift_out = 1.0;
        dcg_prev = dcg;
   end
 
   //---------------------------------------------------
   // For analog debug only (uncomment if required)
   // Step1: Accumlation
   // Step2: Voltage generation
   //---------------------------------------------------
   
   //nvhs_sreal_monitor #(.testclk_period(1ps)) sreal_monitor_chan (.svin(svo), .en(1'b1)); //single-ended

endmodule
`endif
`ifdef BEHAV_CTLE_LTE_MFLF_EXT_SV
module behav_ctle_lte_mflf
   import types_pkg::*;
 ( svin, svo, mfgain, mfpole, lfgain, lfpole, i_ctrl, hfgain, res_ctrl, level,
   dcshift_out, dcshift_in, dcshift_clr, term_lpf_pole );

    timeunit        1ns;
    timeprecision   1ps;

    input   model   level;

    //PINS
    input   sreal   svin;
    output  sreal   svo;

    input   real    dcshift_in;
    input           dcshift_clr;
    output  real    dcshift_out;

    //2-state integer
    input   int     mfgain;                  //mf gain tuning
    input   int     mfpole;                  //mf pole tuning
    input   int     lfgain;                  //lf gain tuning
    input   int     lfpole;                  //lf pole tuning
    input   int     i_ctrl;                  //ictrl tuning

   //2-state integer
   input    int     hfgain;
   input    int     res_ctrl;
   input    int     term_lpf_pole;

   int tia_ctrl;
   //hfgain, res_ctrl, i_ctrl
   //    0, 3, 0:1     => 0
   // 1:12, 3, 0:1     => 1
   //    0, 2, 1:2     => 2
   // 1:12, 2, 1:2     => 3
   //    0, 1, 2:4     => 4
   // 1:12, 1, 2:4     => 5 
   //    0, 0, 4:7     => 6 
   // 1:12, 0, 4:7     => 7
   always_comb begin
        if      ( (hfgain===0) && (res_ctrl===3) && (i_ctrl>=0) && (i_ctrl<=1) )                tia_ctrl = 0;
        else if ( (hfgain>=1) && (hfgain<=12) && (res_ctrl===3) && (i_ctrl>=0) && (i_ctrl<=1) ) tia_ctrl = 1;
        else if ( (hfgain===0) && (res_ctrl===2) && (i_ctrl>=1) && (i_ctrl<=2) )                tia_ctrl = 2;
        else if ( (hfgain>=1) && (hfgain<=12) && (res_ctrl===2) && (i_ctrl>=1) && (i_ctrl<=2) ) tia_ctrl = 3;
        else if ( (hfgain===0) && (res_ctrl===1) && (i_ctrl>=2) && (i_ctrl<=4) )                tia_ctrl = 4;
        else if ( (hfgain>=1) && (hfgain<=12) && (res_ctrl===1) && (i_ctrl>=2) && (i_ctrl<=4) ) tia_ctrl = 5;
        else if ( (hfgain===0) && (res_ctrl===0) && (i_ctrl>=4) && (i_ctrl<=7) )                tia_ctrl = 6;
        else if ( (hfgain>=1) && (hfgain<=12) && (res_ctrl===0) && (i_ctrl>=4) && (i_ctrl<=7) ) tia_ctrl = 7;
        else                                                                                    tia_ctrl = 8; //invalid
   end 

   real ztia_LUT[8:0] = '{0.0, 158.86375, 159.40429, 380.18940, 380.18940, 896.39619, 896.39619, 3126.07937, 3126.07937};
   
    //---------------------------------------------------
    // SREAL Coefficient and LUT
    //---------------------------------------------------
    // Original TF: 
    // Gm_(lf|mf) = gm/(1+gm*Rs) /  (Rl*Cl*s + 1)
    // lfn|mfn = gm/(1+gm*Rs)
    // lfd|mfd = Rl*Cl
    // Added a pole wc at 100e12 rad/s to avoid constant after partial fraction Exp
    // Target TF : c0/(s+wp0) + c1/(s+wp1) + c2/(s+wp2)
    //---------------------------------------------------

    real s19, s18, s17, s16, s15, s14, s13, s12, s11, s10, s9, s8, s7, s6, s5, s4, s3, s2, s1;
    real c0, wp0, c1, wp1, c2, wp2;
    real gm_lf, gm_mf, c_lpflf, c_lpfmf, r_slf, r_smf;
    real lfd, mfd, lfn, mfn;
 
    real gm_mf_LUT [0:7]  ='{ 1.28276988e-3, 1.49240201e-3, 1.81320102e-3, 2.18708297e-3, 2.51320249e-3, 3.05366342e-3, 3.48278050e-3, 3.83153885e-3};
    real r_smf_LUT [0:7]  ='{ 154.326040, 214.161043, 397.083562, 604.379655, 851.385798, 1.47797008e3, 2.79216303e3, 67.1392771e3};
    real c_lpfmf_LUT [0:7]='{ 134.982690e-15, 121.348491e-15, 107.492909e-15, 93.29854e-15, 78.7609535e-15, 63.7407434e-15, 48.1035548e-15, 31.9467479e-15};
    real r_lpfmf          =        2431.0;

    real gm_lf_LUT [0:7]  ='{ 1.81359985e-3, 2.26302071e-3, 3.09274508e-3, 4.365734004e-3, 5.89187565e-3, 10.0702842e-3, 16.9625318e-3, 30.4708046e-3};
    real r_slf_LUT [0:7]  ='{ 334.3922602, 397.1072395, 594.1164203, 823.6603698, 1099.648602, 1801.156548, 3274.083591, 95289.6623};
    real c_lpflf_LUT [0:7]='{ 186.667149e-15, 167.960356e-15, 149.516199e-15, 113.321978e-15, 95.7020003e-15, 78.3031672e-15, 61.3330677e-15, 131.260409e-15};

    real r_lpflf          =        7565.6;

    //real wc               =        36e9; //36e9 VGA Pole
    real lpf_pole [0:3] = '{ 6e9, 9e9, 13e9, 1e18}; //in Hz
    real wc;
    assign wc             = 2.0*3.142*lpf_pole[term_lpf_pole]; //1e18 CTLE fake Pole
    real ztia;              //ZTIA gain 162.18


    //always @* begin
    always_comb begin
        ztia    = ztia_LUT[tia_ctrl];
        gm_lf   = gm_lf_LUT[i_ctrl]; // * gm_ratio_LUT[lte_lowpwr];
        c_lpflf = c_lpflf_LUT[lfpole];
        r_slf   = r_slf_LUT[lfgain];
        gm_mf   = gm_mf_LUT[i_ctrl]; // * gm_ratio_LUT[lte_lowpwr];
        c_lpfmf = c_lpfmf_LUT[mfpole];
        r_smf   = r_smf_LUT[mfgain];
        lfn     = gm_lf/(1+gm_lf*r_slf);
        mfn     = gm_mf/(1+gm_mf*r_smf);
        lfd     = r_lpflf*c_lpflf;
        mfd     = r_lpfmf*c_lpfmf;
        s19 = lfd*mfn;
        s18 = lfn*mfd;
        s17 = lfn*mfn;
        s16 = lfd*mfd;
        s14 = s16*mfd;
        s13 = lfd**2*mfd;
        s15 = s13*mfd;
        s12 = ztia;
        s11 = s12**2; 
        s10 = s12*wc**2;
        s9  = s10*s12;
        s8  = lfd+mfd+s19*s12+s18*s12;
        s7  = -(s16*wc**2);
        s6  = s11*(s19**2+2*s16*s17+s18**2)+s12*(2*lfd*s19-2*s16*lfn-2*s16*mfn+2*s18*mfd)+lfd**2-2*s16+mfd**2;
        s5  = 2*s16*s7*s6;
        s4  = s8+s6**0.5;
        s3  = s8-s6**0.5;
        s2  = (s13*(s17*s9-lfn*s10+mfn**2*s9+mfn*s10)+s14*(lfn**2*s9+s17*s9+lfn*s10-mfn*s10))/(s7*s6);
        s1  = -s9*(s19*s13*mfn+2*s15*s17+s14*lfn*s18)-s10*(s19*s13-s15*lfn-s15*mfn+s14*s18);

        c0  <= wc;
        wp0 <= wc;
        c1  <= -(s2+s3*s1/s5);
        wp1 <= s3/(2*s16);
        c2  <= -(s2+s4*s1/s5);
        wp2 <= s4/(2*s16);
    end

    //---------------------------------------------------
    // SREAL Calculation. svin*H(tf) 
    //---------------------------------------------------
    complex    c[$];     // CTLE coefficients
    complex   wp[$];     // CTLE poles
    complex    b[$];     // Input poles, 0,1, ..k-1
    complex    p[$];     // Incoming poles
    complex    d[$];     // Output coefficients
    complex wout[$];     // Output poles
    int         k=0;

    initial initialize_sreal(svo);

    //---------------------------------------------------------------------------
    bit coeff_trig=1'b0;
    bit init_trig=1'b0;

    real d0_prev=0, d0_new=0, dcshift=1.0;

    always @(c0 or c1 or c2 or wp0 or wp1 or wp2 or posedge dcshift_clr) begin
        d0_new = c0/wp0 + c1/wp1 + c2/wp2;
        if (init_trig & (d0_prev != 0.0) & ~dcshift_clr) dcshift = d0_new/d0_prev;
        else           dcshift = 1.0;
        d0_prev = d0_new;
        if (~dcshift_clr) coeff_trig=1'b1 & init_trig;
    end

    assign dcshift_out = dcshift_in * dcshift;

    always @(c0 or c1 or c2 or wp0 or wp1 or wp2)
        coeff_trig=1'b1 & init_trig;
    //---------------------------------------------------------------------------

    always @(svin.trig or posedge coeff_trig) begin
        case(level)

        SIMPLE : begin svo = svin; end

        FULL, MEDIUM   : begin
                 b = {}; p = {};          
                 foreach (svin.c[i])  if (isX(svin.c[i].Re)) break; else begin b = {b, svin.c[i]}; p = {p, svin.w0[i]}; end
                 c  = {}; c.push_back('{c0,0}); c.push_back('{c1,0});  c.push_back('{c2,0});
                 wp = {}; wp.push_back('{wp0,0}); wp.push_back('{wp1,0}); wp.push_back('{wp2,0}); 
                 k = 0;

                 foreach (b[i]) begin
                    d[k] = '{0,0};
                    foreach (c[j]) begin
                        d[k] = sum(d[k], div(c[j], sub(wp[j],p[i]) ));
                    end
                    d[k] = mult(d[k], b[i]);
                    wout[k++]  = p[i];
                 end //foreach b[i]

                 foreach (c[j]) begin
                    d[k] = '{0,0};
                    foreach (b[i]) begin
                        d[k] = sum(d[k], div(b[i], sub(p[i],wp[j]) ));
                    end
                    d[k] = mult(d[k],c[j]);
                    wout[k++]  = wp[j]; 
                 end //foreach c[j]

                 foreach(d[l]) begin
                     svo.c[l] = d[l];
                     svo.w0[l] = wout[l];
                 end //foreach d[l]

                 if (coeff_trig) begin coeff_trig=0; svo.t = $realtime*1e-9; end else svo.t = svin.t;

                 svo.trig = ~svo.trig;
                 init_trig = 1'b1;

                 end

        default: begin svo = svin; end

        endcase
    end

    //---------------------------------------------------
    // For analog debug only (uncomment if required)
    // Step1: Accumlation
    // Step2: Voltage generation
    //---------------------------------------------------

    //nvhs_sreal_monitor #(.testclk_period(2ps)) sreal_monitor_lte (.svin(svo), .en(1'b1));

endmodule
`endif
`ifdef BEHAV_CTLE_TAS_HGDC_EXT_SV
module behav_ctle_tas_hgdc
   import types_pkg::*;
#( parameter bit NOISE_MODEL = 1'b0)
 ( svin, svo, hfgain, tas_r, i_ctrl,
   gmeff0, level, dcshift_in, dcshift_out, dcshift_clr );

    timeunit        1ns;
    timeprecision   1ps;

    input   model   level;

    //PINS
    input   sreal   svin;
    output  sreal   svo;
 
    input   real    dcshift_in;
    input           dcshift_clr;
    output  real    dcshift_out;

    // 2-state integer
    input   int     tas_r;         //Rs tuning 0-7
    input   int     hfgain;        //Cs tuning 0-12
    input   int     i_ctrl;        //Current tuning 0-3

    // Output Real
    output  real    gmeff0;        //dc gain

    //---------------------------------------------------
    // SREAL Coefficient and LUT
    //---------------------------------------------------
    // Original TF: (s*Cs*Rs*Gm + Gm) / (s*Rs*Cs + Rs*Gm + 1)
    // Target TF : c0/(s+wp0) + c1/(s+wp1)
    // c0 = Wc*(Gm - Wc*Cs*Rs*Gm) / (Rs*Gm-Wc*Cs*Rs+1)
    // c1 = Wc*Rs*Gm^2 / (Rs*Gm-Wc*Cs*Rs+1)
    // wp0 = Wc
    // wp1 = (Rs*Gm+1)/(Cs*Rs)
    //----------------------------------------------------

    //real Wc            =      52e9; //term pole

    //complex Z1       = '{1.62924126042425e+012,  1.25941282409543e+012};
    real    Z1M        = 2.05926e12;
    //complex Z2       = '{493.999021403827e+009, -381.845662149963e+009};
    real    Z2M        = 6.24373e11;
    //complex P1       = '{78.8311098089673e+009, 142.489857036676e+009};
    real    P1M        = 1.62843e11;
    //complex P2       = '{63.5830178663973e+009,-114.928297530604e+009};
    real    P2M        = 1.31344e11;
    real    Wc         = 13.4062074626452e+027;

    //real Rs_LUT [7:0]  = '{ 101.5109e3, 28.9170e3, 12.2769e3, 10.5640e3, 6.6782e3, 5.6090e3, 4.8033e3, 4.1973e3}; //TODO Check order

    real Rs_LUT [7:0] = '{127.064197467781e3, 35.9451171121067e3, 15.0641353868276e3, 12.9150338710400e3, 8.03956445482086e3, 6.69810472845886e3, 5.68734406708636e3, 4.92712198691402e3};

    real Cs_LUT [0:12] = '{ 2.3541e-15, 2.4129e-15, 2.8541e-15, 3.2800e-15, 4.0783e-15, 4.8864e-15, 5.7103e-15, 6.8267e-15,
                            7.9548e-15, 9.6490e-15, 11.0954e-15, 12.8934e-15, 14.7388e-15};

    //1.82005415512568e-015    1.68010157709405e-015    2.05044972594500e-015    2.40972020246458e-015    3.09319285215077e-015    3.79996449697535e-015    4.53523509088035e-015    5.55058975089290e-015
    //6.59234860739639e-015    8.15434623828595e-015    9.50775205706014e-015    11.1936787368243e-015    12.9250665038626e-015

    real Gm_LUT [0:7]  = '{194.784350740983e-6, 214.572747819590e-6, 245.753200839452e-6, 280.459939290334e-6, 303.322016686847e-6, 329.820995127787e-6, 354.661920068231e-6, 369.216056574142e-6}; 
    // '{184.991689396060e-6, 206.112412248482e-6, 237.628702181564e-6, 270.851440564989e-6, 294.624140280600e-6, 321.705348395360e-6, 336.498633749820e-6, 349.415959851767e-6};
    //'{ 248.4720e-6, 278.8858e-6, 325.0450e-6, 374.7780e-6, 411.1567e-6, 453.3748e-6, 476.7681e-6, 497.3879e-6};

    complex c0n, c0d, c0;
    complex c1n, c1d, c1;
    complex c2n, c2d, c2;
    complex c3n, c3d, c3;
    complex wp0, wp1, wp2, wp3;

    real gmeff0i;
    real Rs, Cs, Gm, A1, B1;
    real A1xGm, A1xB1xGm, B1xGm, B1xB1xGm;
    real B1xB1xB1xGm, A1xB1xB1xGm, B1xB1, B1xB1xB1;
    real c2n_a, c2d_a;

    always_comb begin
        Rs          = Rs_LUT[tas_r];
        Cs          = Cs_LUT[hfgain];
        Gm          = Gm_LUT[i_ctrl];
        A1          = 1/Cs/Rs;
        B1          = (Rs*Gm+1)*A1;
        gmeff0i      = Z1M*Z2M*Gm/P1M/P2M/(1+Rs*Gm); //TODO
        A1xGm       = A1*Gm;
        A1xB1xGm    = A1xGm*B1;
        B1xGm       = B1*Gm;
        B1xB1xGm    = B1xGm*B1;
        B1xB1xB1xGm = B1xB1xGm*B1;
        A1xB1xB1xGm = B1xB1xGm*A1;
        B1xB1       = B1*B1;
        B1xB1xB1    = B1xB1*B1;

        c2n_a       = 33554432.0*A1xB1xB1xGm - 33554432.0*B1xB1xB1xGm;
        c2d_a       = 8589934592.0*B1xB1xB1-2.46305909879e60;

        c0n   = mult('{-3.4319891104e30,0.0}, sum(mult('{Gm,0.0},'{8.0847705554e91, -5.29227163689e75}), mult('{A1xGm,0.0},'{-6.03061721813e63, 3.94762773263e47})));
        c0d   = sum(mult('{B1,0.0},'{1.54383800784e66, -3.17394634086e48}),'{-2.06970126218e94,4.25505831209e76});

        c1n   = mult('{8.04372447759e28,0.0},sum(mult('{Gm,0.0},'{1.37612970588e49,1.38519738167e49}), mult('{A1xGm,0.0},'{-1.15341214192e38, 3.27659893443e37})));
        c1d   = sum(mult('{B1,0.0},'{1.15077713811e53,1.94274095623e54}),'{2.67749177218e65,-1.6954583264e65});

        c2n   = mult('{-3.4319891104e30,0.0}, sum('{c2n_a,0.0}, sum(mult('{B1xGm,0.0},'{-4.31424402196e31,-1.00645706211e27}), sum(mult('{B1xB1xGm,0.0},'{7.1244121656e19,2.94462676609e19}), sum(mult('{A1xGm,0.0},'{4.31424402196e31,1.00645706211e27}), mult('{A1xB1xGm,0.0},'{-7.12441216563e19,-2.94462676609e19}))))));
        c2d   = sum(mult('{B1,0.0},'{1.6400189522e49,3.17394634086e48}), sum(mult('{B1xB1,0.0},'{-1.15158445231e38,-2.36751993411e20}), '{c2d_a,-1.13810523198e54}));

        c3n   = mult('{-1.34062074626e28,0.0},sum(mult('{Gm,0.0},'{1.93942833887e50,-2.4447293331e50}), mult('{A1xGm,0.0},'{-2.34349281824e39,-3.90995856729e38})));
        c3d   = sum(mult('{B1,0.0},'{4.60310855246e53,7.77096382492e54}), '{-9.22371595903e65,-4.4119858879e65});

        c0   <= div(c0n,c0d);
        wp0  <= '{1.34062074626e28,0.0};

        c1   <= div(c1n, c1d);
        wp1  <= '{78831109809.0,142489857037.0};
        
        c2   <= div(c2n, c2d);
        wp2  <= '{B1,0.0};

        c3   <= div(c3n, c3d);
        wp3  <= '{63583017866.4, -114928297531.0};
    end

    assign gmeff0 = gmeff0i;

    //always_comb begin
    //    case(level)
    //        SIMPLE       : gmeff0 = 9.756e-3; //Worst case dcg is used here
    //        FULL, MEDIUM : gmeff0 = gmeff0i;
    //        default      : gmeff0 = 9.756e-3;
    //    endcase
    //end
    
    //---------------------------------------------------
    // SREAL Calculation. svin*H(tf)
    //---------------------------------------------------
    complex    c[$];     // CTLE coefficients
    complex   wp[$];     // CTLE poles
    complex    b[$];     // Input poles, 0,1, ..k-1
    complex    p[$];     // Incoming poles
    complex    d[$];     // Output coefficients
    complex wout[$];     // Output poles
    int         k=0;

    initial initialize_sreal(svo);

    //---------------------------------------------------------------------------
    bit coeff_trig=1'b0;
    bit init_trig=1'b0;

    real d0_prev=0, d0_new=0, dcshift=1.0;

    always @(c0.Re or c1.Re or c2.Re or c3.Re or wp0.Re or wp1.Re or wp2.Re or wp3.Re or
             c0.Im or c1.Im or c2.Im or c3.Im or wp0.Im or wp1.Im or wp2.Im or wp3.Im or
            posedge dcshift_clr) begin
        d0_new  = ((c0.Re*wp0.Re + c0.Im*wp0.Im)/(wp0.Re*wp0.Re+wp0.Im*wp0.Im)) +
                  ((c1.Re*wp1.Re + c1.Im*wp1.Im)/(wp1.Re*wp1.Re+wp1.Im*wp1.Im)) +
                  ((c2.Re*wp2.Re + c2.Im*wp2.Im)/(wp2.Re*wp2.Re+wp2.Im*wp2.Im)) +
                  ((c3.Re*wp3.Re + c3.Im*wp3.Im)/(wp3.Re*wp3.Re+wp3.Im*wp3.Im));
        if (init_trig & (d0_prev != 0.0) & ~dcshift_clr) dcshift = d0_new/d0_prev;
        else           dcshift = 1.0;
        d0_prev = d0_new;
        //if (~dcshift_clr) coeff_trig=1'b1 & init_trig;
    end

    always @(c0.Re or c1.Re or c2.Re or c3.Re or wp0.Re or wp1.Re or wp2.Re or wp3.Re or
             c0.Im or c1.Im or c2.Im or c3.Im or wp0.Im or wp1.Im or wp2.Im or wp3.Im)
		coeff_trig = 1'b1 & init_trig;


    assign dcshift_out = dcshift_in * dcshift;
    //---------------------------------------------------------------------------

    always @(level or svin.trig or posedge coeff_trig) begin
        case(level)
        SIMPLE : begin 
            if (~NOISE_MODEL)
                svo = svin;
        end
  
        FULL, MEDIUM : begin

            if (NOISE_MODEL) begin
                 c  = {}; c.push_back(c0); c.push_back(c1); c.push_back(c2); c.push_back(c3); 
                 wp = {}; wp.push_back(wp0); wp.push_back(wp1);  wp.push_back(wp2); wp.push_back(wp3); 
                 foreach (c[i]) begin
                    svo.c[i]         = c[i];
                    svo.w0[i]        = wp[i];
                 end
                 coeff_trig=0; 
                 svo.t = $realtime*1e-9;
                 svo.trig = ~svo.trig;
                 init_trig = 1'b1;
            end

            else begin
                 
                 b = {}; p = {};          
                 foreach (svin.c[i])  if (isX(svin.c[i].Re)) break; else begin b = {b, svin.c[i]}; p = {p, svin.w0[i]}; end
                 c  = {}; c.push_back(c0); c.push_back(c1); c.push_back(c2); c.push_back(c3); 
                 wp = {}; wp.push_back(wp0); wp.push_back(wp1);  wp.push_back(wp2); wp.push_back(wp3); 
                 k = 0;
                 foreach (b[i]) begin
                     d[k] = '{0,0};
                     foreach (c[j]) begin
                         d[k] = sum(d[k], div(c[j], sub(wp[j],p[i]) ));
                     end
                     d[k] = mult(d[k], b[i]);
                     wout[k++]  = p[i];
                 end //foreach b[i]
                 foreach (c[j]) begin
                     d[k] = '{0,0};
                     foreach (b[i]) begin
                         d[k] = sum(d[k], div(b[i], sub(p[i],wp[j]) ));
                     end
                     d[k] = mult(d[k],c[j]);
                     wout[k++]  = wp[j]; 
                 end //foreach c[j]
                 foreach(d[l]) begin
                     svo.c[l] = d[l];
                     svo.w0[l] = wout[l];
                 end //foreach d[l]
    
                 if (coeff_trig) begin coeff_trig=0; svo.t = $realtime*1e-9; end else svo.t = svin.t;

                 svo.trig = ~svo.trig;
                 init_trig = 1'b1;
            end
        end

        default: begin
            if (~NOISE_MODEL) 
            svo = svin;
        end
        endcase
    end
   
    //---------------------------------------------------
    // For analog debug only (uncomment if required)
    // Step1: Accumlation
    // Step2: Voltage generation
    //---------------------------------------------------

    //nvhs_sreal_monitor #(.testclk_period(2ps)) sreal_monitor_tas (.svin(svo), .en(1'b1));

endmodule
`endif
`ifdef BEHAV_CTLE_TIA_EXT_SV
module behav_ctle_tia
   import types_pkg::*;
 ( svin, svo, hfgain, res_ctrl, i_ctrl, level,  dcshift_in, dcshift_out, dcshift_clr );

   timeunit         1ns;
   timeprecision    1ps;

   input    model   level;

   //PINS
   input    sreal   svin;
   output   sreal   svo;

   input    real    dcshift_in;
   input            dcshift_clr;
   output   real    dcshift_out;

   //2-state integer
   input    int     hfgain;
   input    int     res_ctrl;
   input    int     i_ctrl;

   int tia_ctrl;
   //hfgain, res_ctrl, i_ctrl
   //    0, 3, 0:1     => 0
   // 1:12, 3, 0:1     => 1
   //    0, 2, 1:2     => 2
   // 1:12, 2, 1:2     => 3
   //    0, 1, 2:4     => 4
   // 1:12, 1, 2:4     => 5 
   //    0, 0, 4:7     => 6 
   // 1:12, 0, 4:7     => 7
   always_comb begin
        if      ( (hfgain===0) && (res_ctrl===3) && (i_ctrl>=0) && (i_ctrl<=1) )                tia_ctrl = 0;
        else if ( (hfgain>=1) && (hfgain<=12) && (res_ctrl===3) && (i_ctrl>=0) && (i_ctrl<=1) ) tia_ctrl = 1;
        else if ( (hfgain===0) && (res_ctrl===2) && (i_ctrl>=1) && (i_ctrl<=2) )                tia_ctrl = 2;
        else if ( (hfgain>=1) && (hfgain<=12) && (res_ctrl===2) && (i_ctrl>=1) && (i_ctrl<=2) ) tia_ctrl = 3;
        else if ( (hfgain===0) && (res_ctrl===1) && (i_ctrl>=2) && (i_ctrl<=4) )                tia_ctrl = 4;
        else if ( (hfgain>=1) && (hfgain<=12) && (res_ctrl===1) && (i_ctrl>=2) && (i_ctrl<=4) ) tia_ctrl = 5;
        else if ( (hfgain===0) && (res_ctrl===0) && (i_ctrl>=4) && (i_ctrl<=7) )                tia_ctrl = 6;
        else if ( (hfgain>=1) && (hfgain<=12) && (res_ctrl===0) && (i_ctrl>=4) && (i_ctrl<=7) ) tia_ctrl = 7;
        else                                                                                    tia_ctrl = 8; //invalid
   end 

   real c0_Re_LUT [8:0] = '{ 1.0e-6, 16.2574e+012, -3.5303e+012, 29.5794e+012, -17.8563e+012, -14.5298e+012, -6.5887e+012, 43.9147e+012, 15.6181e+012};
   real c0_Im_LUT [8:0] = '{    0.0, 0.0000e+000, -5.7456e+012, 0.0000e+000, -2.9595e+012, -12.6587e+012, 799.0733e+009, 0.0000e+000, 0.0000e+000};

   real c1_Re_LUT [8:0] = '{ 1.0e-6, 3.1317e+012, -3.5303e+012, -2.4325e+012, -17.8563e+012, -14.5298e+012, -6.5887e+012, -89.8541e+012, -25.1147e+012};
   real c1_Im_LUT [8:0] = '{    0.0, -10.4911e+012, 5.7456e+012, -15.1592e+012, 2.9595e+012, 12.6587e+012, -799.0733e+009, 0.0000e+000, 0.0000e+000};

   real c2_Re_LUT [8:0] = '{ 1.0e-6, 3.1317e+012, 27.8682e+012, -2.4325e+012, 53.1768e+012, 58.9334e+012, 33.4790e+012, 80.0481e+012, 32.0415e+012};
   real c2_Im_LUT [8:0] = '{    0.0, 10.4911e+012, 0.0000e+000, 15.1592e+012, 0.0000e+000, 0.0000e+000, 0.0000e+000, 0.0000e+000, 0.0000e+000};

   //real c3_Re_LUT [8:0] = '{ 1.0e-6, 
   //real c3_Im_LUT [8:0] = '{    0.0, 

   real wp0_Re_LUT [8:0] = '{ 2.2222e16, 383.3069e+009, 110.9519e+009, 164.3297e+009, 105.9742e+009, 48.6137e+009, 58.3108e+009, 69.7153e+009, 74.5662e+009};
   real wp0_Im_LUT [8:0] = '{    0.0, 0.0000e+000, -213.4617e+009, 0.0000e+000, -86.5654e+009, -63.1571e+009, -69.3929e+009, 0.0000e+000, 0.0000e+000};

   real wp1_Re_LUT [8:0] = '{ 3.3333e16, 76.7349e+009, 110.9519e+009, 61.6501e+009, 105.9742e+009, 48.6137e+009, 58.3108e+009, 45.9686e+009, 51.9249e+009}; 
   real wp1_Im_LUT [8:0] = '{    0.0, -168.4437e+009, 213.4617e+009, -97.3714e+009, 86.5654e+009, 63.1571e+009, 69.3929e+009, 0.0000e+000, 0.0000e+000};

   real wp2_Re_LUT [8:0] = '{ 4.4444e16, 76.7349e+009, 213.6719e+009, 61.6501e+009, 95.9125e+009, 68.0052e+009, 33.3789e+009, 18.0135e+009, 9.4349e+009};
   real wp2_Im_LUT [8:0] = '{    0.0, 168.4437e+009, 0.0000e+000, 97.3714e+009, 0.0000e+000, 0.0000e+000, 0.0000e+000, 0.0000e+000, 0.0000e+000};

   //real wp3_Re_LUT [8:0] = '{ 1.0e15, 
   //real wp3_Im_LUT [8:0] = '{    0.0, 

   complex c0, c1, c2, c3, wp0, wp1 ,wp2, wp3;

   always_comb begin
      c0  <= '{c0_Re_LUT[tia_ctrl],c0_Im_LUT[tia_ctrl]};
      c1  <= '{c1_Re_LUT[tia_ctrl],c1_Im_LUT[tia_ctrl]};
      c2  <= '{c2_Re_LUT[tia_ctrl],c2_Im_LUT[tia_ctrl]};
      //c3  <= '{c3_Re_LUT[tia_ctrl],c3_Im_LUT[tia_ctrl]};
      wp0 <= '{wp0_Re_LUT[tia_ctrl],wp0_Im_LUT[tia_ctrl]};
      wp1 <= '{wp1_Re_LUT[tia_ctrl],wp1_Im_LUT[tia_ctrl]};
      wp2 <= '{wp2_Re_LUT[tia_ctrl],wp2_Im_LUT[tia_ctrl]};
      //wp3 <= '{wp3_Re_LUT[tia_ctrl],wp3_Im_LUT[tia_ctrl]};
   end

   bit coeff_trig = 1'b0;
   bit init_trig  = 1'b0;

   real d0_prev=0, d0_new=0, dcshift=1.0;
   //always @(c0.Re or c1.Re or c2.Re or c3.Re or wp0.Re or wp1.Re or wp2.Re or wp3.Re or c0.Im or c1.Im or c2.Im or c3.Im or wp0.Im or wp1.Im or wp2.Im or wp3.Im or posedge dcshift_clr) begin
   always @(c0.Re or c1.Re or c2.Re or wp0.Re or wp1.Re or wp2.Re or c0.Im or c1.Im or c2.Im or wp0.Im or wp1.Im or wp2.Im or posedge dcshift_clr) begin
      d0_new = ((c0.Re*wp0.Re + c0.Im*wp0.Im)/(wp0.Re*wp0.Re+wp0.Im*wp0.Im)) +
               ((c1.Re*wp1.Re + c1.Im*wp1.Im)/(wp1.Re*wp1.Re+wp1.Im*wp1.Im)) +
               ((c2.Re*wp2.Re + c2.Im*wp2.Im)/(wp2.Re*wp2.Re+wp2.Im*wp2.Im));
               //((c3.Re*wp3.Re + c3.Im*wp3.Im)/(wp3.Re*wp3.Re+wp3.Im*wp3.Im));
      if (init_trig & (d0_prev != 0.0) & ~dcshift_clr) dcshift = d0_new/d0_prev;
      else           dcshift = 1.0;
      d0_prev = d0_new;
      //if (~dcshift_clr) coeff_trig = 1'b1 & init_trig;
   end

   assign dcshift_out = dcshift_in * dcshift;

   always @(c0.Re or c1.Re or c2.Re or wp0.Re or wp1.Re or wp2.Re or c0.Im or c1.Im or c2.Im or wp0.Im or wp1.Im or wp2.Im)
      coeff_trig = 1'b1 & init_trig;
    
   //---------------------------------------------------
   // SREAL Calculation. svin*H(tf)
   //---------------------------------------------------
   complex    c[$];     // CTLE coefficients
   complex   wp[$];     // CTLE poles
   complex    b[$];     // Input poles, 0,1, ..k-1
   complex    p[$];     // Incoming poles
   complex    d[$];     // Output coefficients
   complex wout[$];     // Output poles
   int         k=0;

   initial initialize_sreal(svo);

   always @(svin.trig or posedge coeff_trig) begin
      case(level)

      SIMPLE : begin
         svo = svin;
      end

      FULL, MEDIUM : begin
         b = {}; p = {};          
         foreach (svin.c[i])  if (isX(svin.c[i].Re)) break; else begin b = {b, svin.c[i]}; p = {p, svin.w0[i]}; end
         c  = {}; c.push_back(c0); c.push_back(c1); c.push_back(c2);// c.push_back(c3);
         wp = {}; wp.push_back(wp0); wp.push_back(wp1); wp.push_back(wp2);// wp.push_back(wp3);
         k = 0;

         foreach (b[i]) begin
            d[k] = '{0,0};
            foreach (c[j]) begin
               d[k] = sum(d[k], div(c[j], sub(wp[j],p[i]) ));
            end
            d[k] = mult(d[k], b[i]);
            wout[k++]  = p[i];
         end //foreach b[i]

         foreach (c[j]) begin
            d[k] = '{0,0};
            foreach (b[i]) begin
               d[k] = sum(d[k], div(b[i], sub(p[i],wp[j]) ));
            end
            d[k] = mult(d[k],c[j]);
            wout[k++]  = wp[j]; 
         end //foreach c[j]

         foreach(d[l]) begin
             svo.c[l] = d[l];
             svo.w0[l] = wout[l];
         end //foreach d[l]
        
         if (coeff_trig) begin coeff_trig=0; svo.t = $realtime*1e-9; end else svo.t = svin.t;
         svo.trig = ~svo.trig;
         init_trig = 1'b1;
      end

      default : begin
         svo = svin;
      end

      endcase
   end
//---------------------------------------------------
// For analog debug only (uncomment if required)
// Step1: Accumlation
// Step2: Voltage generation
//---------------------------------------------------

   //nvhs_sreal_monitor #(.testclk_period(2ps)) sreal_monitor_zload (.svin(svo), .en(1'b1));

endmodule
`endif
`ifndef RX_CTLE_CORE_EXT_SV
module rx_ctle_core 
    import types_pkg::*;
    ( 
    input  logic [3:0]  ctle_vicm_selbb,
    input  logic 	ctle_bias_enbb_hv,
    input  logic 	ctle_enbb_hv,
    input  logic [2:0] 	ctle_tas_r_thermbb,
    input  logic 	AGND,
    input  logic 	tie_lo_rvdd,
    output logic 	ctle_atest_v,
    output logic 	ctle_vicm,
    input  logic [2:0] 	ctle_atest_selb_hv,
    input  logic [6:0] 	ctle_lf_gain_therm,
    output logic 	ctle_cal_vp,
    input  logic 	ctle_bias_enb_hv,
    input  logic [3:0] 	ctle_vicm_selb,
    input  logic [6:0] 	ctle_mf_cap_therm,
    input  logic 	ctle_enb_hv,
    input  logic 	ctle_vos_cal_enbb,
    input  logic 	HVDD,
    input  logic [30:0] ctle_vos_selp,
    input  logic [7:1] 	ctle_atest_selbb,
    input  logic [3:0] 	ctle_vcm_selbb,
    input  logic 	ctle_hf_gain_modeb,
    input  logic 	data_rvdd,
    output logic 	ctle_cal_vn,
    input  logic [6:0] 	ctle_lf_cap_therm,
    input  logic [1:0] 	ctle_rload_rcal_selb,
    input  logic [11:0] ctle_hf_gain_thermb,
    input  logic 	ctle_irefp,
    input  logic 	ctle_hf_gain_modebb,
    input  logic [30:0] ctle_vos_seln,
    input  logic 	ctle_vos_cal_enb,
    input  logic [6:0] 	ctle_mf_gain_therm,
    output logic 	ctle_cal_vcm,
    output sreal	ctle_vp,
    input  logic [2:0] 	ctle_rload_thermbb,
    input  logic 	ctle_vos_rangeb,
    output logic 	ctle_vcm,
    output sreal	ctle_vn,
    input  logic 	ctle_vos_rangebb,
    input  sreal 	term_vp,
    input  sreal	term_vn,
    input  logic [1:0] 	ctle_tas_r_rcal_selb,
    input  logic 	ctle_ptat_irefp,
    input  logic [1:0] 	ctle_i_ctrlbb_hv,
    input  logic [7:1] 	ctle_atest_selb,
    input  logic [11:0] ctle_hf_gain_therm,
    input  logic [3:0] 	ctle_vcm_selb
);

timeunit 1ns;
timeprecision 10fs;


// Place holder until model is ready
// Plan to descend one more level
// Input sreal, output sreal
sreal vin;
sreal vo;
// No need for initialize_sreal() beacuse the variables are driven from input
always @(term_vp.trig or term_vn.trig) begin
   foreach (term_vp.c[i]) begin vin.c[i] = sub(term_vp.c[i],term_vn.c[i]); end
   vin.t    = term_vp.t;
   vin.w0   = term_vp.w0;
   vin.m    = term_vp.m;
   vin.trig = term_vp.trig;
end
//TODO: CTLE is pass through.
assign vo = vin;
assign ctle_vicm = 1'b1;
assign ctle_vcm = 1'b1;

///*
///////////////////////
// Normally, will send sreal vo to SAH downstream and evaluate vo at each SAH clock
// But the computation for accumulation is repeated unnecesarrily
// To save compuations  pass the accumulated coeff to following stage as this is the last stage with sreal
//////////////////////
real 	delT, prev_t=0.0, ph, alpha;
int 		L;
complex atten;
complex a[$];
complex coeff[$]; // Need to intialize c[] with '{0,0} of size vin.c.size() or real initializes to 0 anyway??
always @(vo.trig)
  begin
     delT = vo.t - prev_t;
     a={};
     foreach (vo.c[i]) begin 
         if (~`isX(vo.c[i].Re)) begin
             a = {a, vo.c[i]};
         end else begin
             break;
         end
     end    
     L = a.size();
     for (int ii=0; ii<=L-1; ii++) 
       begin
          alpha = exp(-vo.w0[ii].Re*delT); ph = -vo.w0[ii].Im*delT; atten = '{alpha*cos(ph),alpha*sin(ph)};
          coeff[ii] = sum( mult(coeff[ii],atten), vo.c[ii]); // Differential voltage
       end
     prev_t = vo.t;
     foreach (coeff[i]) begin
        ctle_vp.c[i]  = mult( coeff[i],'{+0.5,0} ) ; ctle_vn.c[i]  = mult( coeff[i],'{-0.5,0} );
        ctle_vp.w0[i] = vo.w0[i]                   ; ctle_vn.w0[i] = vo.w0[i]                  ;
        ctle_vp.m[i]  = vo.m[i]                    ; ctle_vn.m[i]  = vo.m[i]                   ;
     end
     ctle_vp.t = vo.t                              ; ctle_vn.t = vo.t                          ;
     ctle_vp.trig = ~ctle_vp.trig                  ; ctle_vn.trig = ~ctle_vn.trig              ;
  end   
///////////////////////// Evaluation block ///////////////////////////////////////
// Oversampling  waveform, v(t), for display. Use for debug. Comment out otherwise
///////////////////////////////////////////////////////////////////////////////////
/*
 
bit zclk = 1'b0;
real zt, zvrx=0, zphi, zdecay, zRe, zIm;
always #1ps zclk = ~zclk;   
always @(posedge zclk) 
  begin
     zt = $realtime*1e-9 - vo.t; // Time since last event
     zvrx = 0.0;
     foreach (ctle_vp.c[j])
       begin
          // 2*c[j] for Differential signal
          zRe = 2*ctle_vp.c[j].Re; zIm = 2*ctle_vp.c[j].Im; zdecay = exp(-ctle_vp.w0[j].Re*zt); zphi = -ctle_vp.w0[j].Im*zt;
          zvrx   +=  zdecay *( zRe*cos(zphi) - zIm*sin(zphi) );
       end
  end 
 */
endmodule

`endif  // RX_AFE_EXT_SV

`ifdef SREAL_MULT_EXT_SV
module sreal_mult
  import types_pkg::*;
#( parameter bit NOISE_MODEL = 1'b0)
 ( svin, svin_coeff, svo, dcshift_in, dcshift_clr, ctle_en, level );

    timeunit 1ns;
    timeprecision 1ps;

    //PINS
    input   sreal   svin;
    input   sreal   svin_coeff;
    output  sreal   svo;
    input   real    dcshift_in;
    output          dcshift_clr;
    input           ctle_en;
    input   model   level;

    //---------------------------------------------------
    // SREAL Calculation. svin*H(tf)
    //---------------------------------------------------
    complex    c[$];     // CTLE coefficients
    complex   wp[$];     // CTLE poles
    complex    b[$];     // Input poles, 0,1, ..k-1
    complex    p[$];     // Incoming poles
    complex    d[$];     // Output coefficients
    complex wout[$];     // Output poles
    int         k=0;
    
    initial initialize_sreal(svo);

    bit coeff_trig=1'b0;
    bit init_trig=1'b0;
    real prev_time;
    real dcshift = 0.0;
    real v=0;
    bit dcshift_clr = 1'b1;
    bit dcshift_trig = 1'b0;
    //TODO Check if the coeff will pass thru at the time 0.
    always @(svin_coeff.trig) begin
	#1ps;
	/* Glitch Reduction
        if (init_trig) begin @(svin.trig); prev_time = svin.t; end
        while(init_trig) begin
            fork 
                #10ps;
                @(svin.trig);
            join_any
            disable fork;
            if (!(prev_time < svin.t)) break;
           else prev_time = svin.t;
        end
    */
        if (init_trig) dcshift = v*(dcshift_in - 1.0);
        else           dcshift = 0;
        c = {}; wp = {};
        foreach (svin_coeff.c[i]) begin
            if (isX(svin_coeff.c[i].Re)) break;
            else begin c = {c, svin_coeff.c[i]}; wp = {wp, svin_coeff.w0[i]}; end
        end
        coeff_trig=1'b1 & init_trig;
    end

    //---------------------------------------------------------------------------

    always @(svin.trig or posedge coeff_trig or negedge ctle_en) begin
     if (ctle_en) begin
       if (NOISE_MODEL & (level===SIMPLE))
            svo = svin;
       else begin

         b = {}; p = {};          
         foreach (svin.c[i])  if (isX(svin.c[i].Re)) break; else begin b = {b, svin.c[i]}; p = {p, svin.w0[i]}; end
         k = 0;
         foreach (b[i]) begin
             d[k] = '{0,0};
             foreach (c[j]) begin
                 d[k] = sum(d[k], div(c[j], sub(wp[j],p[i]) ));
             end
             d[k] = mult(d[k], b[i]);
             wout[k++]  = p[i];
         end //foreach b[i]
         foreach (c[j]) begin
             d[k] = '{0,0};
             foreach (b[i]) begin
                 d[k] = sum(d[k], div(b[i], sub(p[i],wp[j]) ));
             end
             d[k] = mult(d[k],c[j]);
             wout[k++]  = wp[j]; 
         end //foreach c[j]
         foreach(d[l]) begin
             if (coeff_trig) begin
                 svo.w0[l] = svo.w0[l];
                 if (l==0) svo.c[l] = '{dcshift,0};
                 else svo.c[l] = '{0,0};
             end else begin
                 svo.c[l] = d[l];
                 svo.w0[l] = wout[l];
             end
          end //foreach d[l]
    
          v += svo.c[0].Re;

          if (coeff_trig) begin dcshift_trig = ~dcshift_trig; coeff_trig=1'b0; svo.t = $realtime*1e-9; end else svo.t = svin.t;

          svo.trig = ~svo.trig;
          init_trig = 1'b1;

        end
      end
      else begin
         init_trig = 1'b0;
      end
    end
   
    always @(dcshift_trig or ctle_en) begin
		dcshift_clr = 1'd1;
		dcshift_clr = #1ps 1'd0 | ~ctle_en;
    end
 
    //---------------------------------------------------
    // For analog debug only (uncomment if required)
    // Step1: Accumlation
    // Step2: Voltage generation
    //---------------------------------------------------

    //nvhs_sreal_monitor #(.testclk_period(2ps)) sreal_monitor_tas (.svin(svo), .en(1'b1));

   
endmodule
`endif
`ifdef SREAL_REAL2SREAL_EXT_SV
module sreal_real2sreal
  import types_pkg::*;
 (vin, svo );

    timeunit 1ns;
    timeprecision 10fs;

    input   real    vin;
    output  sreal   svo;

    initial initialize_sreal(svo);

    real deltaV =0, prev_vin =0;

    always @(vin) begin
        deltaV      = vin - prev_vin;
        svo.c[0]    = complex'{deltaV,0.0};
        svo.w0[0]   = complex'{0.0 ,0.0};
        //svo.m[0]    = 1;
        svo.t       = $realtime*1e-9;
        prev_vin    = vin;
        svo.trig    = ~svo.trig;          
    end

endmodule
`endif
`ifdef SREAL_SUM_EXT_SV
module sreal_sum
  import types_pkg::*;
 ( svin_d, svin_n, svo, level );

    timeunit 1ns;
    timeprecision 1ps;

    //PINS
    input   sreal   svin_n;
    input   sreal   svin_d;
    output  sreal   svo;
    input   model   level;

    initial initialize_sreal(svo);

    int chp; //number of channel poles
    int ctp; //number of ctle poles
    always_comb begin
        case(level)
            SIMPLE  : begin chp = 3;  ctp =  0; end
            MEDIUM  : begin chp = 12; ctp = 10; end
            FULL    : begin chp = 48; ctp = 10; end
            default : begin chp = 3;  ctp =  0; end
        endcase
    end

   always @(svin_d.trig)
     begin
        foreach(svo.c[i]) begin
            svo.c[i] = svin_d.c[i]; svo.w0[i] = svin_d.w0[i];
        end
	if (svin_d.w0[0] === svin_n.w0[0]) begin
	      svo.c[0]      = sum(svin_d.c[0], svin_n.c[0]);
    end
	for (int k=1;k<(ctp+1);k++) begin
            if (svin_d.w0[k+chp] === svin_n.w0[k])
	    svo.c[k+chp] = sum(svin_d.c[k+chp], svin_n.c[k]); 
        end
        svo.t    = $realtime * 1e-9; //svin_d.t;
        svo.trig = ~svo.trig;
   end

endmodule
`endif
///home/tools/cadence/ICADV123.709/tools.lnx86/dfII/etc/smg/bbtDefinitions/smg_EE_pkg.sv
///home/tools/cadence/INCISIV_15.20.005/tools/affirma_ams/etc/dms/EE_pkg_examples/

`ifndef TYPES_PKG_SEEN
`define TYPES_PKG_SEEN

package types_pkg;
   // parameters
   parameter twoPi= 6.283185307179586;
   const real realZ= $bitstoreal(64'hFFFFFFFF00000000);
   const real realX= $bitstoreal(64'hFFFFFFFFFFFFFFFF);

   function automatic bit  isZ(real x);  
       return ($realtobits(x) === 64'hFFFFFFFF00000000);
   endfunction
   function automatic bit  isX(real x);  
       return ($realtobits(x) === 64'hFFFFFFFFFFFFFFFF);
   endfunction
   function automatic bit isXZ(real x);  
       return  (isX(x)|| isZ(x));
   endfunction

   function automatic int modName_to_i(string modName);
       reg   [511:0]     modName_trunc;
       begin
            modName_trunc = modName;
            modName_to_i = modName_trunc % 32'hffff_ffff;
       end
   endfunction

   typedef enum 	 { SIMPLE = 'd1, MEDIUM = 'd2, FULL   = 'd3 } model;
   
   // Complex number is Re+j*Im
   typedef struct 	 { 
 			   real Re;
 			   real Im;
 			   } complex;

`ifdef OFFSET_NOISE
   logic 				rand_en = 1'b1;
`else
   logic 				rand_en = 1'b0;
`endif
   
`ifdef CH_FULL
   typedef struct 		{
 				 real      t       ;
 				 complex   c  [61];
 				 complex   w0 [61];
 				 //shortint  m  [61];
 				 bit       trig    ;
 				 } sreal;
   model level = FULL;
   `elsif CH_MEDIUM
     typedef struct 		    {
 				     real      t       ;
 				     complex   c  [25];
 				     complex   w0 [25];
 				     //shortint  m  [25];
 				     bit       trig    ;
 				     } sreal;
   model level = MEDIUM;
   `elsif CH_SIMPLE
     typedef struct 		    {
 				     real      t       ;
 				     complex   c  [4];
 				     complex   w0 [4];
 				     //shortint  m  [4];
 				     bit       trig    ;
 				     } sreal;
   model level = SIMPLE;
`else // Default
   typedef struct 			       {
 						real      t       ;
 						complex   c  [4];
 						complex   w0 [4];
 						//shortint  m  [4];
 						bit       trig    ;
 						} sreal;
   model level = SIMPLE;
`endif // !`ifdef CH_FULL
//`endif //  `ifndef TYPES_PKG_SEEN
   
   // Struct to define Voltage, current, and resistance for the electrical nettype:

   typedef struct 	      {real V;
 			       real I;
 			       real R;
 			       }EEstruct;

   typedef EEstruct rx_pad[1:0]; //rxp,rxn

   task initialize_sreal (output sreal y) ;
      begin
	 foreach (y.c[i]) begin y.c[i] = '{realX,realX}; y.w0[i] = '{realX,realX}; end // y.m[i] = 1; end
      end
   endtask // initialize_sreal

   // Resolution function
//   function automatic sreal res_sreal (input sreal driver[]);
//      real 	tt;
//      complex   tc[200]     = '{default:0};
//      complex   tw0[200]    = '{default:0};
//      shortint 	tm[200]     = '{default:0};
//      bit 	ttrig       = 0;
//      begin
//         foreach(driver[i]) begin
//            tt    = driver[i].t;
//            tc    = driver[i].c;
//            tw0   = driver[i].w0;
//            tm    = driver[i].m;
//            ttrig = driver[i].trig;
//         end
//         res_sreal.t    = tt;
//         res_sreal.c    = tc;
//         res_sreal.w0   = tw0;
//         res_sreal.m    = tm;
//         res_sreal.trig = ttrig;
//      end
//   endfunction // res_sreal

   // Function to covert string to complex
   function void cmdstr2complex(
				output complex C[$], 
				input byte real_separator, 
				input byte struct_separator,
				input string in);
      automatic int Re_index [$]; // queue
      automatic int Im_index [$]; // queue
      automatic string Re_temp[];
      automatic string Im_temp[];
      int return_code;
      static bit 			     find_real_sep = 1'b1;
      static bit 				     find_struct_sep = 1'b0;
      real Re, Im;
      
      if(in.len > 0 ) begin
         if(C.size != 0) C.delete();
         foreach(in[i]) begin // find commas
            if (in[i]==real_separator & find_real_sep ) begin
               Im_index.push_back(i-1); // index before comma
               Im_index.push_back(i+1); // index after comma
               find_struct_sep = 1'b1;
               find_real_sep   = 1'b0;
               continue;
            end
            if (in[i]==struct_separator & find_struct_sep) begin
               Re_index.push_back(i-1); // index before collon
               Re_index.push_back(i+1); // index after collon
               find_struct_sep = 1'b0;
               find_real_sep   = 1'b1;
               continue;
            end
         end
         Re_index.push_front(0); // first index
         Im_index.push_back(in.len()-1); // last index
      end
      
      if(Re_index.size() === Im_index.size()) begin
         if(Re_index.size >0) begin
            Re_temp = new[Re_index.size()/2];
         end
         if(Im_index.size >0) begin
            Im_temp = new[Im_index.size()/2];
         end
         foreach (Re_temp[i]) begin
            Re_temp[i] = in.substr(Re_index[2*i],Im_index[2*i]);
            Im_temp[i] = in.substr(Im_index[2*i+1],Re_index[2*i+1]);
         end
         // Remove the unnecessary ASCII characters
         foreach (Re_temp[i,j]) begin
            if( Re_temp[i][j] == "'" || Re_temp[i][j] == "}"  || 
                Re_temp[i][j] == "{" || Re_temp[i][j] == "\"" || 
                Re_temp[i][j] == "\n" ) begin
               Re_temp[i][j] = " ";
            end
         end
         foreach (Im_temp[i,j]) begin
            if( Im_temp[i][j] == "'" || Im_temp[i][j] == "}"  || 
                Im_temp[i][j] == "{" || Im_temp[i][j] == "\"" || 
                Im_temp[i][j] == "\n" ) begin
               Im_temp[i][j] = " ";
            end
         end
	 
         // Chomp to remove the spaces from the string
         foreach(Re_temp[i]) begin
            while(Re_temp[i].substr(0,0) == " " || Re_temp[i].substr(0,0) == "\t") begin
               Re_temp[i] = Re_temp[i].substr(1, Re_temp[i].len() -1);
            end
            while(Re_temp[i].substr(Re_temp[i].len-1,Re_temp[i].len-1) == " ") begin
               Re_temp[i] = Re_temp[i].substr(0, Re_temp[i].len() -2);
            end
         end
         foreach(Im_temp[i]) begin
            while(Im_temp[i].substr(0,0) == " " || Im_temp[i].substr(0,0) == "\t") begin
               Im_temp[i] = Im_temp[i].substr(1, Im_temp[i].len() -1);
            end
            while(Im_temp[i].substr(Im_temp[i].len-1,Im_temp[i].len-1) == " ") begin
               Im_temp[i] = Im_temp[i].substr(0, Im_temp[i].len() -2);
            end
         end
         foreach (Re_temp[i]) begin
            //$display("Re_temp[i] = %s Im_temp[i]=%s", Re_temp[i], Im_temp[i]);
            return_code = $sscanf(Re_temp[i], "%f", Re);
            return_code = $sscanf(Im_temp[i], "%f", Im);
            C.push_back('{Re, Im});
         end
      end
      else begin
         $display("The format of input data is not correct");
      end
   endfunction : cmdstr2complex
   
   // `define EE_DEBUG
   
   // Macro to get type of real value: 0=highZ 1=zero 2=value 3=invalid
   // `define rtype(R) ( isZ(R) )? 0 : (R==0)   ? 1 : (R<1e30) ? 2 : 3
   function automatic logic[1:0] rtype( real R);
       return (isZ(R))? 0 : (R==0) ? 1 : (R<1e30) ? 2 : 3;
   endfunction

   function real abs(input real A); abs = (A<0)? -A:A; endfunction
   import "DPI" pure function real cos  (input real rVal);
   import "DPI" pure function real sin  (input real rVal);
   import "DPI" pure function real exp  (input real rVal);
   import "DPI" pure function real sqrt (input real rVal);
    // Function to genereate a random seed

    function int get_rseed();
        int fp;
	begin
        void'($system("date +%N > sys_time.tmp"));
        fp = $fopen("sys_time.tmp","r");
	if (fp!=0) begin
        	void'($fscanf(fp,"%s",get_rseed));
        	$fclose(fp);
        	void'($system("rm sys_time.tmp"));
		end
	else begin
		return $urandom;
		end
	end
    endfunction

      function automatic complex mult (input complex a, input complex b);
	 real x,y;
 	 begin
	    x = a.Re*b.Re;
	    y = a.Im*b.Im;
 	    mult.Re = x - y;
 	    mult.Im = (a.Re+a.Im)*(b.Re+b.Im)-x-y;
 	 end
      endfunction // mult
      function automatic complex sum (input complex a, input complex b);
 	 begin
 	    sum.Re = a.Re + b.Re;
 	    sum.Im = a.Im + b.Im;
 	 end
      endfunction // sum
      function automatic complex sub (input complex a, input complex b);
 	 begin
 	    sub.Re = a.Re - b.Re;
 	    sub.Im = a.Im - b.Im;
 	 end
      endfunction // sub
      function automatic complex div (input complex a, input complex b);
 	 real rb2=1;
	 real x, y, z, d0, d1;
	 complex w='{0,0};
 	 begin
 	    rb2 = b.Re*b.Re + b.Im*b.Im;
 	    w = mult(a, '{b.Re, -b.Im});
 	    div.Re = w.Re/rb2;
 	    div.Im = w.Im/rb2;
 	 end
      endfunction // div
      
      // Electrical "EEnet" nettype with thevenin equivalent resolution
      // Usage:
      //   ideal voltage drive:   V=v1, R=0, (I ignored)
      //   ideal current drive:   I=i1, R=Z and/or V=Z
      //   V+R drive:        V=v1, R=r1, I=0 or Z
      //   I||R drive:           I=i1, R=r1, V=0
      //   Combination:              V=v1, R=r1, I=i1
      //   No drive:                           I=Z and (V=Z or R=Z)
      // Multiple ideal voltage drives will result in X.
      // Ideal currents into large loads or Z will saturate to the
      //   specified VHI or VLO voltages.
      // Undriven nodes will resolve to the specified VZ,RZ values.
      
      function automatic EEstruct res_EE (input EEstruct driver[]);
         const real                  res_realZ = realZ;                                   
         const real                  res_realX = realX;                                   
 	 real 			     VHI=2.01, VLO=-2.01; // output voltage clip limits when overdriven
	 real 			     VZ=res_realZ, RZ=res_realZ;       // resolved voltage & resistance when undriven
	 real 			     IT=0, GT=0;         // summed current & conductance for node
	 real 			     Vsrc=res_realZ;            // value of driving voltage source
	 reg [7:0] 		     numVsrc=0;          // number ideal voltage sources driving pin
	 reg [7:0] 		     numErr=0;           // number erroneous drivers to node
	 reg [7:0] 		     numInp=0;           // number of inputs connected
	 reg [1:0] 		     ty,tv,ti,tr;           // type for each term of each driver
	 real 			     Vnom;               // resolved voltage before limiting
	 logic 			     q = 1'bz;           // Logic type initialized to 1'bz
 	 
	 begin
 	    foreach (driver[i]) begin  // sum current & conductance contributions
 	       numInp+=1;
 	       tv=rtype(driver[i].V);
 	       ti=rtype(driver[i].I);
 	       tr=rtype(driver[i].R);
 	       if (ti==3 || tr==3 || (tv==3 && tr!=0)) numErr+=1;   // invalid
 	       else if (tv==0 || tr==0) begin                       // no V
 		  if (ti==2)  IT+=driver[i].I;                      // I only
 	       end
 	       else if (tr==1) begin                                // ideal V
 		  numVsrc+=1; Vsrc=driver[i].V;
 	       end
 	       else begin                                           // nonzero R
 		  if (ti==2) IT+=driver[i].I;                       // I term
 		  GT+=1/driver[i].R;                                // R term
 		  if (tv==2) IT+=driver[i].V/driver[i].R;           // V/R term
 	       end
 	       //if( (ty==2) || (ty==0 && q === 1'b1) || (ty==1 && q === 1'b0) || (q === 1'bx) ) begin q = 1'bx; end
 	       //else if(ty == 3 && q === 1'bz)                                        begin q = 1'bz; end
 	       //else if(ty == 0 && q === 1'bz)                                        begin q = 1'b0; end
 	       //else if (ty == 1 && q === 1'bz)                                       begin q = 1'b1; end
            //`ifdef EE_DEBUG  $display(" <<<>>> q=%b, ty=%0d", q, ty);
            //`endif
 	    end // foreach (driver[i])
 	    //res_EE.Y = q;
 	    
 	    if (numErr>0 || numVsrc>1) begin                        // improper drive detected
 	       res_EE.V=res_realX;                                         // all errors map to Z for now ... would like to just
 	       res_EE.I=0;                                          // do Z at time zero, X thereafter, but can't access
 	       res_EE.R=RZ;                                         // "time" from a function!
 	    end
 	    else if (numVsrc==1) begin
 	       res_EE.V=Vsrc;                                       // ideal voltage drive
 	       res_EE.I=IT-Vsrc*GT;	                            
 	       res_EE.R=0;		                            
 	    end				                            
 	    else if (GT==0) begin                                   // open circuited node
 	       if (IT==0) begin		                            
 		  res_EE.V=VZ;                                      // no current - return open ckt info
 		  res_EE.I=0;		                            
 		  res_EE.R=RZ;
 	       end			                            
 	       else if (IT>0) begin	                            
 		  res_EE.V=VHI;                                     // positive current - clip to VHI
 		  res_EE.I=0;		                            
 		  res_EE.R=VHI/IT;                                  // estimate large signal dV/dI
 	       end			                            
 	       else begin		                            
 		  res_EE.V=VLO;                                     // negative current - clip to VLO
 		  res_EE.I=0;		                            
 		  res_EE.R=-VHI/IT;                                 // estimate large signal dV/dI
 	       end			                            
 	    end				                            
 	    else begin                                              // normal resolution vis sumI/sumG:
 	       if (IT/GT>VHI) begin	                            
 		  res_EE.V=VHI;                                     // saturated high
 		  res_EE.I=0;		                            
 		  res_EE.R=VHI/IT;                                  // estimate large signal dV/dI
 	       end			                            
 	       else if (IT/GT<VLO) begin                            
 		  res_EE.V=VLO;                                     // saturated low
 		  res_EE.I=0;		                            
 		  res_EE.R=-VHI/IT;                                 // estimate large signal dV/dI
 	       end
 	       else begin
 		  res_EE.V=IT/GT;                                   // normal unsaturated result
 		  res_EE.I=0;		                              
 		  res_EE.R=1/GT;                                    // normal on resistance
 	       end
 	    end
 `ifdef EE_DEBUG
 	    $display("<> V=%5.3f  I=%8.2g  R=%8.2g       typeVIR=%d%d%d  numIVE=%d%d%d",
 		     res_EE.V,res_EE.I,res_EE.R, tv,ti,tr, numInp,numVsrc,numErr);
 	    //$display("<> V=%5.3f  I=%8.2g  R=%8.2g  Y=%b     typeVIR=%d%d%d  numIVE=%d%d%d",
 		     //res_EE.V,res_EE.I,res_EE.R, res_EE.Y, tv,ti,tr, numInp,numVsrc,numErr);
 `endif 
	 end
      endfunction // res_EE
      
      nettype EEstruct EEnet with res_EE;
      nettype          EEnet pad_t;
      nettype          EEnet nv_pad_t;
 
    function automatic void pad_to_logic (
        output  logic [1:0] txp_val,
        output  logic [1:0] txn_val, 
        input   EEstruct txp,
        input   EEstruct txn
    );
 
        {txp_val,txn_val} = (isZ(txp.V) || isZ(txn.V))                        ? {2'bzz,2'bzz}:
                            (isX(txp.V) || isX(txn.V))                        ? {2'bxx,2'bxx}:
                            ((txp.V-txn.V) >= -0.01 && (txp.V-txn.V) <= 0.01) ? {2'b00,2'b00}: // Electrical idle
                            ((txp.V-txn.V) >=  0.666)                         ? {2'b11,2'b00}: // TODO: PAM4 gray encoded data
                            ((txp.V-txn.V) >=  0.0)                           ? {2'b10,2'b01}: // TODO: PAM4 gray encoded data
                            ((txp.V-txn.V) <= -0.666)                         ? {2'b00,2'b11}:
                            ((txp.V-txn.V) <=  0.0)                           ? {2'b01,2'b10}: {2'bxx,2'bxx};
        
    endfunction 
    
    function automatic void logic_to_pad (
      output EEstruct   rxp,
      output EEstruct   rxn,
      input logic [1:0] rxp_val,
      input logic [1:0] rxn_val  
    ); 
      rxp = {rxp_val, rxn_val} === 4'bzzzz ? '{realZ, 0.0, 1e9} :
            {rxp_val, rxn_val} === 4'bxxxx ? '{realX, 0.0, realX} :
            {rxp_val, rxn_val} === 4'b0000 ? '{0.5, 0.0, 43.0} : 
            {rxp_val, rxn_val} === 4'b0011 ? '{0.0, 0.0, 43.0} : 
            {rxp_val, rxn_val} === 4'b1100 ? '{1.0, 0.0, 43.0} : 
            {rxp_val, rxn_val} === 4'b0110 ? '{0.333, 0.0, 43.0} : 
            {rxp_val, rxn_val} === 4'b1001 ? '{0.666, 0.0, 43.0} : 
                                             '{realX, 0.0, realX};
      

      rxn = {rxp_val, rxn_val} === 4'bzzzz ? '{realZ, 0.0, 1e9} :
            {rxp_val, rxn_val} === 4'bxxxx ? '{realX, 0.0, realX} :
            {rxp_val, rxn_val} === 4'b0000 ? '{0.5, 0.0, 43.0} : 
            {rxp_val, rxn_val} === 4'b0011 ? '{1.0, 0.0, 43.0} : 
            {rxp_val, rxn_val} === 4'b1100 ? '{0.0, 0.0, 43.0} : 
            {rxp_val, rxn_val} === 4'b0110 ? '{0.666, 0.0, 43.0} : 
            {rxp_val, rxn_val} === 4'b1001 ? '{0.333, 0.0, 43.0} : 
                                             '{realX, 0.0, realX};

    endfunction

    function automatic void dft_pad_to_logic (
        output  logic [1:0] txp_val,
        output  logic [1:0] txn_val, 
        input   EEstruct txp,
        input   EEstruct txn
    );
      txp_val = isZ(txp.V)     ? 2'bzz :
                isX(txp.V)     ? 2'bxx :
                txp.V >=0.666  ? 2'b11 : 2'b00 ;

      txn_val = isZ(txn.V)     ? 2'bzz :
                isX(txn.V)     ? 2'bxx :
                txn.V >=0.666  ? 2'b11 : 2'b00 ;

//    {txp_val,txn_val} = (isZ(txp.V) || isZ(txn.V))                        ? {2'bzz,2'bzz}:
//                        (isX(txp.V) || isX(txn.V))                        ? {2'bxx,2'bxx}:
//                        ((txp.V>=0.666) && (txn.V>=0.666))                ? {2'b11,2'b11}: 
//                        ((txp.V-txn.V) >= -0.01 && (txp.V-txn.V) <= 0.01) ? {2'b00,2'b00}: // Electrical idle
//                        ((txp.V-txn.V) >=  0.666)                         ? {2'b11,2'b00}: // TODO: PAM4 gray encoded data
//                        ((txp.V-txn.V) >=  0.0)                           ? {2'b10,2'b01}: // TODO: PAM4 gray encoded data
//                        ((txp.V-txn.V) <= -0.666)                         ? {2'b00,2'b11}:
//                        ((txp.V-txn.V) <=  0.0)                           ? {2'b01,2'b10}: {2'bxx,2'bxx};
        
    endfunction 
    
    function automatic void dft_logic_to_pad (
      output EEstruct   rxp,
      output EEstruct   rxn,
      input logic [1:0] rxp_val,
      input logic [1:0] rxn_val  
    ); 
      rxp = rxp_val === 2'bzz ? '{realZ, 0.0, 1e9}   :
            rxp_val === 2'bxx ? '{realX, 0.0, realX} :
            rxp_val === 2'b11 ? '{1.0,   0.0, 43.0}  :
            rxp_val === 2'b00 ? '{0.0,   0.0, 43.0}  : '{realX, 0, realX};

      rxn = rxn_val === 2'bzz ? '{realZ, 0.0, 1e9}   :
            rxn_val === 2'bxx ? '{realX, 0.0, realX} :
            rxn_val === 2'b11 ? '{1.0,   0.0, 43.0}  :
            rxn_val === 2'b00 ? '{0.0,   0.0, 43.0}  : '{realX, 0, realX};

//    rxp = {rxp_val, rxn_val} === 4'bzzzz ? '{realZ, 0.0, 1e9} :
//          {rxp_val, rxn_val} === 4'bxxxx ? '{realX, 0.0, realX} :
//          {rxp_val, rxn_val} === 4'b0000 ? '{0.0, 0.0, 43.0} : 
//          {rxp_val, rxn_val} === 4'b1111 ? '{1.0, 0.0, 43.0} : 
//          {rxp_val, rxn_val} === 4'b0011 ? '{0.0, 0.0, 43.0} : 
//          {rxp_val, rxn_val} === 4'b1100 ? '{1.0, 0.0, 43.0} : 
//          {rxp_val, rxn_val} === 4'b0110 ? '{0.333, 0.0, 43.0} : 
//          {rxp_val, rxn_val} === 4'b1001 ? '{0.666, 0.0, 43.0} : 
//                                           '{realX, 0.0, realX};
//    
//   
//    rxn = {rxp_val, rxn_val} === 4'bzzzz ? '{realZ, 0.0, 1e9} :
//          {rxp_val, rxn_val} === 4'bxxxx ? '{realX, 0.0, realX} :
//          {rxp_val, rxn_val} === 4'b0000 ? '{0.0, 0.0, 43.0} : 
//          {rxp_val, rxn_val} === 4'b1111 ? '{1.0, 0.0, 43.0} : 
//          {rxp_val, rxn_val} === 4'b0011 ? '{1.0, 0.0, 43.0} : 
//          {rxp_val, rxn_val} === 4'b1100 ? '{0.0, 0.0, 43.0} : 
//          {rxp_val, rxn_val} === 4'b0110 ? '{0.666, 0.0, 43.0} : 
//          {rxp_val, rxn_val} === 4'b1001 ? '{0.333, 0.0, 43.0} : 
//                                           '{realX, 0.0, realX};

    endfunction
endpackage // types_pkg

     
 
interface pad_to_logic_interface        
 import types_pkg::*;
(
 input pad_t txp,
 input pad_t txn,
 output logic [1:0] txp_val,
 output logic [1:0] txn_val
 );
//*******************************************************************************//
// USAGE: NRZ mode: Only use the msb bits of txp/txn logic buses in logic_tx_bundle
//        PAM4 mode: use both bits of both txp/txn logic buses in logic_tx_bundle
//*******************************************************************************//

    always_comb begin 
        pad_to_logic(txp_val, txn_val, txp, txn);
    end 
endinterface
   
interface logic_to_pad_interface        
 import types_pkg::*;
(
 output pad_t rxp,
 output pad_t rxn,
 input logic [1:0] rxp_val,
 input logic [1:0] rxn_val
 );
 EEstruct rxp_conv;
 EEstruct rxn_conv;
//*******************************************************************************//
// USAGE: NRZ mode: Only apply the value from bfm to the msb bits of rxp_val/rxn_val logic buses. 
//        Tie off the lsb bits to 0.
//        PAM4 mode: use both bits of both rxp_val/rxn_val logic buses
//        to pass 2bit values per serial clock.
//*******************************************************************************//

    always_comb begin
        logic_to_pad(rxp_conv, rxn_conv, rxp_val, rxn_val);
    end
    assign rxp = rxp_conv;
    assign rxn = rxn_conv;
         
endinterface


`endif //  `ifndef TYPES_PKG_SEEN



