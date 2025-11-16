data <- haven::read_dta("10752_da10_de_v4_0.dta") # 174000 obs, 253 variables

# check if the data contains gew1, gewjahr, bsex, xhrp, dstd, xbalt5, xlfi, wrechtk, xurb, xnuts2, wrechtl, wm2
is.element("gew1", colnames(data))
is.element("gewjahr", colnames(data))
is.element("bsex", colnames(data))
is.element("xhrp", colnames(data))
is.element("dstd", colnames(data))
is.element("xbalt5", colnames(data))
is.element("xlfi", colnames(data))
is.element("wrechtk", colnames(data))
is.element("xurb", colnames(data))
is.element("xnuts2", colnames(data))
is.element("wrechtl", colnames(data))
is.element("wm2", colnames(data))
