import("stdfaust.lib");

aa = library("aanl.lib");

NCh = 2;

process = ef.dryWetMixer(dryWet, variable_softclip);

dryWet = gui_out(vslider("dryWet",100,0,100,1)) /100;

variable_softclip(l,r) = l,r : par(i,2,gainup : function : gaindown : makeup) with {
    function = aa.softclipQuadratic2;
    gainup = _ * ba.db2linear(g);
    gaindown = _ * ba.db2linear(0-g);
    
    g = g_base - g_var;
    g_base = gui_sat(vslider("gain", -10,-40,40,1));

    g_var = l,r :> g_var_filter : comp with{
        
        g_var_filter = fi.highpass(4,hp_freq) : fi.lowpass(2,lp_freq) with{
            hp_freq = gui_filter(vslider("hp_freq[scale:log]",11,1,22000,1));
            lp_freq = gui_filter(vslider("lp_freq[scale:log]",22000,1,22000,1));
        };
        
        comp = co.RMS_compression_gain_mono(strength,thresh,att,rel,knee,prePost) : ba.linear2db <: attach(_,gui_dyn(vbargraph("[9]env",-12,0))) with {
            strength = gui_dyn(vslider("[2]ratio", 4,1,20,1) : ratio2strength);
            thresh = gui_dyn(vslider("[1]g_var Thresh", -20,-40,0,1));
            att = gui_dyn(vslider("[6]attack", 100,1,400,1) / 1000);
            rel = gui_dyn(vslider("[7]release", 400,1,2000,1) / 1000);
            knee = 12;
            prePost = 0;
        };
    };
    

    makeup = _ * makeup_gain with {
        makeup_gain = gui_out(vslider("makeup", -6,-20,20,0.5) : ba.db2linear);
    };
};




// ratio2strength
ratio2strength(ratio) = 1-(1/ratio);

// GUI
gui_main(x) = hgroup("dynSat",x);
gui_sat(x) = gui_main(hgroup("saturation",x));
gui_dyn(x) = gui_main(hgroup("dynamics",x));
gui_filter(x) = gui_main(hgroup("sc filter",x));
gui_out(x) = gui_main(hgroup("out",x));
gui_leveler(x) = gui_main(hgroup("leveler",x));