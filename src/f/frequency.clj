(ns f.frequency)

(def adjacent-probabilities
 {"aa" 3.8320996E-4, "bb" 3.922622E-5, "cc" 3.0174012E-6, "dd" 1.0259164E-4, "ee" 2.6854873E-4, "ff" 0.001037986, "gg" 9.0522044E-5, "hh" 1.9613109E-4, "ii" 9.052204E-6, "kk" 4.5261022E-5, "ll" 0.005944281, "mm" 0.002676435, "nn" 0.0068917447, "oo" 6.638283E-5, "pp" 1.9311368E-4, "rr" 9.2935964E-4, "ss" 0.0063244733, "tt" 0.002082007, "uu" 2.112181E-5, "ww" 1.8707888E-4, "xx" 1.2069605E-5, "zz" 9.052204E-6, "ab" 0.0045502414, "bc" 9.052204E-6, "cd" 6.0348026E-5, "de" 0.022814572, "ef" 0.0011828213, "fg" 5.0994084E-4, "gh" 1.5087006E-5, "hi" 0.0020276937, "kl" 9.414292E-4, "lm" 1.8707888E-4, "mn" 3.3191416E-5, "no" 0.0019492413, "op" 4.164014E-4, "rs" 0.0034941507, "st" 0.014564997, "tu" 0.001894928, "uv" 6.638283E-5, "vw" 2.112181E-5, "xy" 6.0348025E-6, "yz" 3.0174012E-6, "ac" 0.0029027401, "bd" 1.2069605E-5, "ce" 1.1164385E-4, "df" 2.413921E-4, "eg" 0.0028695487, "fh" 3.3191416E-5, "gi" 0.0013819698, "hj" 9.052204E-6, "ik" 5.7632365E-4, "km" 2.413921E-5, "ln" 5.3408E-4, "mo" 9.6255104E-4, "np" 1.2371346E-4, "pr" 0.002278138, "rt" 0.0058839326, "su" 7.6943735E-4, "tv" 1.5992227E-4, "uw" 8.1469836E-5, "ad" 5.9744547E-4, "be" 0.013973585, "dg" 8.1469836E-5, "eh" 0.005226139, "fi" 6.487413E-4, "gj" 3.0174012E-6, "hk" 2.6854873E-4, "il" 0.0027609223, "kn" 7.543503E-5, "lo" 0.0011315255, "mp" 4.556276E-4, "or" 0.0038381345, "ps" 6.638283E-5, "ru" 0.0033040545, "sv" 1.4785267E-4, "tw" 0.0010078121, "ux" 3.0174013E-5, "ae" 0.0012129954, "bf" 1.5087006E-5, "dh" 7.2417635E-5, "ei" 0.025258666, "gk" 3.2286195E-4, "hl" 0.0022057204, "im" 0.0023264166, "ko" 0.002908775, "lp" 3.922622E-5, "nr" 9.0522044E-5, "os" 0.0014604223, "pt" 4.0433177E-4, "qu" 4.0433177E-4, "rv" 8.750464E-5, "sw" 3.9527958E-4, "vz" 6.0348025E-6, "af" 0.0016354315, "bg" 1.9613109E-4, "ch" 0.034096636, "di" 0.012667051, "ej" 1.2069605E-5, "fk" 5.129582E-5, "gl" 0.0016293967, "hm" 0.0011224733, "in" 0.021459758, "jo" 1.5690488E-4, "kp" 6.0348025E-6, "mr" 1.2371346E-4, "ns" 0.0054102005, "ot" 0.0010410035, "pu" 2.564791E-4, "rw" 6.3667167E-4, "ty" 4.224362E-5, "uz" 2.7760092E-4, "ag" 0.002908775, "bh" 1.2069605E-4, "ci" 9.0522044E-5, "dj" 9.052204E-6, "ek" 0.0010741949, "fl" 0.0010259164, "gm" 4.224362E-5, "hn" 0.0016112924, "io" 0.002157442, "jp" 1.2069605E-5, "kq" 1.8104409E-5, "lr" 3.0174013E-5, "ms" 4.13384E-4, "nt" 0.0059774723, "ou" 5.4313223E-5, "rx" 2.413921E-5, "sy" 6.185673E-4, "tz" 0.002172529, "ah" 0.0020518329, "bi" 0.0015690487, "dk" 6.0348026E-5, "el" 0.010935063, "fm" 2.594965E-4, "gn" 1.2974825E-4, "ho" 0.0014905962, "ip" 1.4181786E-4, "kr" 0.0011315255, "ls" 0.0031230105, "mt" 7.8150694E-4, "nu" 0.002208738, "ov" 2.202703E-4, "ry" 8.448724E-5, "sz" 2.7760092E-4, "za" 6.577935E-4, "ai" 2.1725289E-4, "bj" 6.0348026E-5, "ck" 0.0016897448, "dl" 4.6166242E-4, "em" 0.0049606077, "fn" 8.750464E-5, "go" 2.8061832E-4, "hp" 3.922622E-5, "iq" 9.0522044E-5, "ks" 2.293225E-4, "lt" 0.003165254, "mu" 0.0015298225, "nv" 2.6251393E-4, "ow" 1.8104409E-4, "rz" 7.181415E-4, "ya" 6.0348025E-6, "zb" 4.827842E-5, "aj" 1.5087006E-5, "cl" 7.211589E-4, "dm" 8.056462E-4, "en" 0.044862725, "fo" 9.2935964E-4, "gp" 3.0174012E-6, "ir" 0.004453684, "kt" 0.0025285822, "lu" 0.001022899, "mv" 1.2069605E-5, "nw" 1.9613109E-4, "ox" 2.413921E-5, "py" 3.0174012E-6, "xa" 3.3191416E-5, "ak" 9.6858584E-4, "bl" 0.001104369, "cm" 6.0348025E-6, "dn" 1.2069605E-4, "eo" 3.6812297E-4, "fp" 1.8104409E-5, "hr" 0.005159756, "is" 0.007848261, "ku" 0.0010530731, "lv" 6.940023E-5, "mw" 4.224362E-5, "oy" 6.0348025E-6, "wa" 0.0045140325, "yc" 1.5087006E-5, "zd" 8.448724E-5, "al" 0.008134914, "cn" 3.0174012E-6, "do" 9.6858584E-4, "ep" 2.4440952E-4, "gr" 0.0016475011, "hs" 9.2935964E-4, "it" 0.009785432, "ju" 5.7330624E-5, "kv" 1.8104409E-5, "lw" 5.129582E-5, "ny" 2.7156611E-5, "oz" 2.142355E-4, "va" 2.202703E-4, "yd" 9.052204E-6, "ze" 0.0025376345, "am" 0.0019462239, "bn" 7.543503E-5, "co" 2.0518329E-4, "dp" 4.827842E-5, "eq" 8.1469836E-5, "fr" 0.0022509815, "gs" 0.001698797, "ht" 0.00789654, "iu" 6.336543E-5, "kw" 3.3191416E-5, "my" 2.112181E-5, "nz" 0.0015750835, "ua" 2.534617E-4, "xd" 3.0174012E-6, "ye" 3.3191416E-5, "zf" 3.922622E-5, "an" 0.013164922, "bo" 2.6553133E-4, "cp" 3.0174012E-6, "dq" 3.0174012E-6, "er" 0.039905135, "fs" 3.0475753E-4, "gt" 0.0035122551, "hu" 8.26768E-4, "iv" 0.001849667, "ly" 4.5261022E-5, "mz" 2.413921E-5, "ta" 0.0038743434, "ub" 5.129582E-4, "xe" 2.112181E-5, "zg" 2.413921E-5, "ao" 1.5087006E-5, "bp" 6.0348025E-6, "dr" 7.784896E-4, "es" 0.013650724, "ft" 0.0019069976, "gu" 0.0011013515, "hv" 4.5261022E-5, "iw" 2.232877E-4, "ky" 6.0348025E-6, "lz" 1.0560905E-4, "sa" 0.002226842, "tb" 1.7199188E-4, "uc" 0.002827305, "vd" 3.0174012E-6, "we" 0.0078060175, "xf" 3.0174012E-6, "zh" 9.052204E-6, "ap" 6.306369E-4, "cr" 2.413921E-4, "ds" 4.1036657E-4, "et" 0.004450667, "fu" 9.1729E-4, "gv" 9.052204E-6, "hw" 8.4185496E-4, "ix" 6.336543E-5, "kz" 1.3276566E-4, "ra" 0.004999834, "sb" 3.6510557E-4, "tc" 4.827842E-5, "ud" 2.413921E-4, "ve" 0.0049032774, "wf" 3.0174012E-6, "zi" 0.0024471125, "aq" 9.052204E-6, "br" 0.00588695, "cs" 2.112181E-5, "dt" 1.9613109E-4, "eu" 0.0026794523, "gw" 5.4313223E-5, "hx" 3.0174012E-6, "rb" 0.001255239, "sc" 0.009067291, "td" 6.940023E-5, "ue" 0.0025195302, "wg" 6.0348025E-6, "yi" 6.0348025E-6, "ar" 0.007075806, "bs" 6.909849E-4, "ct" 6.336543E-5, "du" 0.0027035917, "ev" 2.1725289E-4, "fw" 1.0560905E-4, "hy" 3.6208818E-5, "iz" 2.504443E-4, "pa" 0.0012974826, "rc" 0.0011224733, "sd" 8.750464E-5, "te" 0.019072995, "uf" 0.0041609965, "vg" 2.112181E-5, "wh" 2.7156611E-5, "xi" 2.1725289E-4, "zk" 9.052204E-6, "as" 0.008783655, "bt" 9.47464E-4, "cu" 6.638283E-5, "dv" 8.1469836E-5, "ew" 0.0010289339, "gy" 1.2069605E-5, "hz" 6.638283E-5, "oa" 3.0174013E-5, "rd" 0.0041308226, "se" 0.009399205, "tf" 1.7199188E-4, "ug" 6.698631E-4, "wi" 0.0066473354, "yk" 1.5087006E-5, "zl" 1.5992227E-4, "at" 0.0048097377, "bu" 7.0908933E-4, "dw" 2.293225E-4, "ex" 0.0011466126, "fy" 3.0174012E-6, "gz" 6.0348025E-6, "na" 0.0028303224, "ob" 0.0010983341, "pc" 1.5087006E-5, "re" 0.011873474, "sf" 2.534617E-4, "tg" 2.353573E-4, "uh" 1.1164385E-4, "vi" 9.47464E-4, "wj" 3.0174012E-6, "xk" 3.0174012E-6, "zm" 1.2069605E-5, "au" 0.010651426, "bv" 7.543503E-5, "ey" 1.4785267E-4, "fz" 8.1469836E-5, "ma" 0.0046196417, "nb" 5.2502786E-4, "oc" 0.0017380231, "pd" 3.6208818E-5, "rf" 8.26768E-4, "sg" 6.547761E-4, "th" 9.6858584E-4, "ui" 1.6293967E-4, "ym" 1.8104409E-5, "zn" 6.0348025E-6, "av" 3.5001856E-4, "bw" 1.2974825E-4, "cx" 6.0348025E-6, "dy" 2.112181E-5, "ez" 4.858016E-4, "la" 0.005045095, "mb" 5.129582E-5, "nc" 1.5690488E-4, "od" 0.0018738062, "pe" 7.9659396E-4, "rg" 0.0018858758, "sh" 3.0777493E-4, "ti" 0.005428305, "wl" 4.224362E-5, "yn" 1.8104409E-4, "zo" 9.655684E-5, "aw" 6.0348025E-6, "cy" 1.8104409E-5, "dz" 2.413921E-5, "ka" 0.0036872644, "lb" 8.901334E-4, "nd" 0.013080435, "oe" 8.3582016E-4, "pf" 5.2804523E-4, "rh" 0.0011104037, "si" 0.006547761, "tj" 2.112181E-5, "uk" 3.2889674E-4, "vl" 6.0348025E-6, "yo" 6.0348025E-6, "zp" 1.2069605E-5, "ax" 5.7330624E-5, "by" 1.8104409E-5, "cz" 3.0174012E-6, "ja" 0.0014272309, "kb" 6.940023E-5, "lc" 4.495928E-4, "md" 5.129582E-5, "ne" 0.011203611, "of" 8.810812E-4, "pg" 1.2069605E-5, "ri" 0.0033945765, "tk" 6.0348026E-5, "ul" 0.0012884304, "wn" 1.5087006E-5, "xo" 1.5087006E-5, "yp" 4.827842E-5, "ay" 1.5087006E-5, "bz" 2.0216589E-4, "ia" 3.6510557E-4, "kc" 2.413921E-5, "ld" 0.005023973, "me" 0.006713718, "nf" 0.0014091264, "og" 5.069234E-4, "ph" 1.3578306E-4, "rj" 1.5087006E-5, "sk" 4.526102E-4, "tl" 9.987599E-4, "um" 0.003856239, "wo" 0.0016414663, "xp" 1.2673085E-4, "zr" 3.6208818E-5, "az" 3.1682715E-4, "ha" 0.0065206042, "ib" 0.0012944652, "kd" 3.0174012E-6, "le" 0.008276732, "mf" 1.5992227E-4, "ng" 0.0075193644, "oh" 0.0013397262, "pi" 8.478898E-4, "rk" 0.0020246764, "sl" 3.8019256E-4, "tm" 1.7802668E-4, "un" 0.012627824, "vo" 0.0038170128, "yr" 6.0348025E-6, "zs" 3.0174013E-5, "ga" 0.0013819698, "hb" 1.5992227E-4, "ic" 0.014824493, "ke" 0.0037868386, "lf" 3.2889674E-4, "mg" 1.2673085E-4, "nh" 3.7114037E-4, "oi" 2.7156611E-5, "rl" 0.0016082749, "sm" 4.495928E-4, "tn" 1.3578306E-4, "uo" 3.6208818E-5, "vp" 1.5087006E-5, "ys" 6.2158465E-4, "zt" 7.8150694E-4, "fa" 0.0018526844, "gb" 5.129582E-5, "id" 9.716032E-4, "je" 0.0013970569, "kf" 5.7330624E-5, "lg" 5.008886E-4, "mh" 1.2069605E-5, "ni" 0.005793411, "oj" 2.413921E-5, "rm" 0.0011013515, "sn" 9.0522044E-5, "to" 0.001345761, "up" 4.375232E-4, "yt" 1.5087006E-5, "zu" 0.005389079, "ea" 8.6297677E-4, "fb" 1.6897448E-4, "hd" 8.1469836E-5, "ie" 0.019338526, "kg" 1.9311368E-4, "lh" 2.112181E-5, "mi" 0.0045502414, "nj" 1.5087006E-5, "ok" 2.9570534E-4, "pl" 3.9226218E-4, "qm" 6.0348025E-6, "rn" 0.0022992599, "so" 0.0048399116, "tp" 2.8363572E-4, "uq" 3.0174012E-6, "vr" 3.0174012E-6, "ws" 6.0348025E-6, "xt" 8.2073314E-4, "zv" 5.129582E-5, "da" 0.010515643, "eb" 0.0027609223, "gd" 2.7156611E-5, "he" 0.010971271, "if" 3.8320996E-4, "kh" 3.6208818E-5, "li" 0.006043855, "nk" 0.002917827, "ol" 0.0024169385, "pm" 3.0174012E-6, "ro" 0.0025708259, "sp" 0.0017319884, "ur" 0.004200223, "vs" 3.922622E-5, "wt" 9.052204E-6, "xu" 3.0174012E-6, "zw" 9.836728E-4, "ca" 1.8707888E-4, "db" 9.0522044E-5, "ec" 0.0016897448, "fd" 6.0348025E-6, "ge" 0.021746412, "hf" 1.1164385E-4, "ig" 0.005600297, "jh" 1.5087006E-5, "ki" 1.6897448E-4, "lj" 2.112181E-5, "mk" 9.052204E-6, "nl" 5.2201044E-4, "om" 0.0021634768, "pn" 3.0174012E-6, "rp" 2.0518329E-4, "sq" 6.0348025E-6, "tr" 0.0026643653, "us" 0.005165791, "vt" 1.8104409E-5, "wu" 7.724547E-4, "ba" 0.0024199558, "dc" 1.5087006E-5, "ed" 0.0027760093, "fe" 0.0028303224, "gf" 5.4313223E-5, "hg" 1.3578306E-4, "ih" 0.0015509443, "ji" 9.052204E-6, "lk" 3.5605335E-4, "ml" 8.509072E-4, "nm" 2.8665314E-4, "on" 0.007232711, "po" 6.306369E-4, "rq" 6.0348025E-6, "sr" 2.082007E-4, "ts" 0.0027729918, "ut" 0.00298421, "vu" 2.112181E-5, "zy" 2.112181E-5})
