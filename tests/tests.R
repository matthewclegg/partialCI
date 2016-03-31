all.tests.pass <- TRUE
all.tests.error.count <- 0

library(zoo)
library(xts)

test <- function(expr, out="", val=eval.parent(parse(text=expr), 1), tol=1e-4) {
    # expr is a string representing an R expression, and
    # out is the output that is expected.  Prints and evaluates
    # expr.  If out is given and it matches the output of
    # evaluating expr, returns TRUE.  Otherwise, returns FALSE.
    
    cat(expr, "-> ")
    
    p <- function (v) {
        if (length(v) < 5) {
            cat(v)
        } else {
            cat(class(v), "(", length(val), ")")
        }
    }
    p(val)
        
    result <- all.equal(val, out, tolerance=tol)
    if (!isTRUE(result)) {
        if (!missing(out)) {
            cat(" (Expecting ")
            p(out)
            cat(")")
        }
        cat("\nERROR: ", result, "\n")
        all.tests.pass <<- FALSE
        all.tests.error.count <<- all.tests.error.count + 1
    } else {
        cat(" OK\n")
    }
    
    isTRUE(result)
}

assert <- function (expr, out) {
    # expr is astring representing an R expression,
    # and out is the output that is expected.  Prints
    # and evaluates expr.  If out matches the output of
    # evaluating expr, returns TRUE.  Otherwise, stops
    # the execution with an error message.
    if (!test(expr, out)) {
        stop("Expression ", deparse(substitute(expr)), 
             " does not evaluate to its expected value\n")
    }
}

test_support <- function () {
  test("partialCI:::ctext(\"\", 0)", "")
  test("partialCI:::ctext(\"\", 1)", " ")
  test("partialCI:::ctext(\"hello\", 0)", "")
  test("partialCI:::ctext(\"hello\", 1)", "h")
  test("partialCI:::ctext(\"hello\", 5)", "hello")
  test("partialCI:::ctext(\"hello\", 6)", "hello ")
  test("partialCI:::ctext(\"hello\", 7)", " hello ")
  test("partialCI:::ctext(\"hello\", 9)", "  hello  ")
}

OIL <- structure(c(95.14, 93.66, 93.12, 93.31, 91.9, 91.36, 92.39, 91.45, 
            92.15, 93.78, 93.54, 93.96, 94.51, 96.35, 97.23, 96.66, 95.82, 
            97.49, 97.34, 98.25, 97.55, 96.44, 97.24, 97.4, 97.84, 99.98, 
            100.12, 99.96, 100.38, 100.27, 100.31, 102.54, 103.46, 103.2, 
            102.53, 103.17, 102.2, 102.93, 102.68, 102.88, 105.34, 103.64, 
            101.75, 101.82, 102.82, 101.39, 100.29, 98.29, 98.57, 99.23, 
            98.43, 100.08, 100.71, 99.68, 99.97, 100.05, 99.66, 100.61, 101.25, 
            101.73, 101.57, 99.69, 99.6, 100.29, 101.16, 100.43, 102.57, 
            103.55, 103.37, 103.68, 104.05, 103.7, 103.71, 104.33, 104.35, 
            101.69, 101.47, 102.2, 100.85, 101.13, 101.56, 100.07, 99.69, 
            100.09, 99.74, 99.81, 101.06, 100.52, 100.32, 100.89, 102.01, 
            102.63, 101.74, 102.31, 102.95, 102.8, 104.31, 104.03, 105.01, 
            104.78, 103.37, 104.26, 103.4, 103.07, 103.34, 103.27, 103.17, 
            103.32, 105.09, 105.02, 105.04, 107.2, 107.49, 107.52, 106.95, 
            106.64, 107.08, 107.95, 106.83, 106.64, 107.04, 106.49, 106.46, 
            106.07, 106.06, 105.18, 104.76, 104.19, 104.06, 102.93, 103.61, 
            101.48, 101.73, 100.56, 101.88, 103.84, 103.83, 105.34, 104.59, 
            103.81, 102.76, 105.23, 105.68, 104.91, 104.29, 98.23, 97.86, 
            98.26, 97.34, 96.93, 97.34, 97.61, 98.09, 97.36, 97.57, 95.54, 
            97.3, 96.44, 94.35, 96.4, 93.97, 93.61, 95.39, 95.78, 95.82, 
            96.44, 97.86, 92.92, 95.5, 94.51, 93.32, 92.64, 92.73, 91.71, 
            92.89, 92.18, 92.86, 94.91, 94.33, 93.07, 92.43, 91.46, 91.55, 
            93.6, 93.59, 95.55, 94.53, 91.17, 90.74, 91.02, 89.76, 90.33, 
            88.89, 87.29, 85.76, 85.87, 85.73, 81.72, 81.82, 82.33, 82.8, 
            82.76, 83.25, 80.52, 82.81, 81.27, 81.26, 81.36, 82.25, 81.06, 
            80.53, 78.77, 77.15, 78.71, 77.87, 78.71, 77.43, 77.85, 77.16, 
            74.13, 75.91, 75.64, 74.55, 74.55, 75.63, 76.52, 75.74, 74.04, 
            73.7, 65.94, 68.98, 66.99, 67.3, 66.73, 65.89, 63.13, 63.74, 
            60.99, 60.01, 57.81, 55.96, 55.97, 56.43, 54.18, 56.91, 55.25, 
            56.78, 55.7, 54.59, 53.46, 54.14, 53.45, 52.72, 50.05, 47.98, 
            48.69, 48.8, 48.35, 46.06, 45.92, 48.49, 46.37, 48.49, 46.79, 
            47.85, 45.93, 45.26, 44.8, 45.84, 44.08, 44.12, 47.79, 49.25, 
            53.04, 48.45, 50.48, 51.66, 52.99, 107.94, 106.57, 106.71, 107.01, 
            107.42, 107.49, 106.44, 108.02, 107.12, 108.09, 107.46, 108.45, 
            109.17, 109.69, 109.69, 109.14, 108.72, 109.1, 108.83, 109.36, 
            108.16, 106.55, 107.04, 106.81, 108.15, 110.12, 110.18, 109.21, 
            108.62, 108.98, 108.63, 110.14, 110.37, 109.42, 109.03, 109.76, 
            109.19, 109.39, 108.54, 108.98, 111.26, 109.17, 108.15, 107.99, 
            109.14, 108.27, 108.35, 107.88, 107.48, 108.08, 106.99, 106.79, 
            105.95, 105.73, 107.2, 106.59, 107.01, 105.9, 106.58, 106.64, 
            105.95, 105.7, 103.37, 104.88, 106.41, 104.89, 105.83, 107.39, 
            107.1, 107.34, 107.68, 109.1, 109.71, 109.79, 109.69, 108.54, 
            108.48, 109.79, 109.53, 109.12, 109.89, 108.63, 108.63, 109.48, 
            109.48, 108.3, 108.17, 108.19, 108.26, 108.37, 108.78, 109.87, 
            109.74, 110.9, 110.84, 110.35, 111.32, 110.89, 110.19, 109.81, 
            109.09, 109.98, 109.21, 109.34, 108.87, 109.07, 108.43, 109.21, 
            110.55, 109.18, 109.83, 112.18, 113.15, 113.42, 114.02, 114.25, 
            115.19, 114.55, 113.62, 113.74, 112.84, 112.61, 112.62, 111.03, 
            110.84, 110.18, 108.98, 108.7, 107.65, 106.84, 106.2, 105.77, 
            104.73, 104.73, 105.41, 106.04, 106.03, 105.71, 106.48, 106.85, 
            105.78, 106.89, 106.7, 106.98, 106.47, 104.94, 103.45, 103.63, 
            102.82, 104.17, 104.02, 103.36, 103.47, 101.68, 102.27, 101.15, 
            101.13, 99.37, 99.74, 99.92, 100.28, 100.09, 100.49, 100.5, 100.4, 
            100.71, 101.12, 100.21, 100.88, 101.21, 99.51, 99.53, 98.08, 
            96.26, 96.42, 96.31, 96.43, 97.39, 97.7, 96.82, 96.75, 95.37, 
            94.87, 94.53, 95.2, 95.08, 95.7, 94.67, 94.57, 91.29, 90.8, 90.65, 
            90.9, 90.25, 90.47, 88.66, 87.82, 86.36, 84.02, 84.02, 85.27, 
            84.42, 85.17, 86.38, 85.94, 86, 85.64, 85.57, 86.91, 85.5, 84.17, 
            84.9, 82.12, 82.88, 82.08, 83.2, 82.9, 80.94, 80.42, 77.74, 77.51, 
            76.86, 77.23, 77.21, 77.61, 79.2, 79.62, 77.62, 77.39, 71.89, 
            70.87, 71.13, 70.13, 68.48, 68, 65.64, 66.11, 63.32, 63.65, 61.67, 
            61.09, 60.26, 59.84, 58.81, 58.87, 58.31, 59.07, 58.67, 58.72, 
            57.86, 55.6, 55.27, 55.38, 51.08, 50.12, 49.06, 49.43, 47.64, 
            46.9, 45.13, 45.82, 47.66, 47.38, 46.49, 46.5, 46.09, 46.69, 
            46.07, 46.55, 47.07, 46.61, 47.52, 51.74, 54.41, 55.07, 55.98, 
            55.88, 57, 26.95, 26.63, 26.52, 26.45, 26.14, 26.14, 26.27, 25.94, 
            26.07, 26.52, 26.44, 26.47, 26.74, 27.08, 26.95, 26.9, 26.71, 
            27.06, 27.05, 27.11, 26.88, 26.71, 26.92, 26.98, 27.31, 27.78, 
            27.73, 27.8, 27.94, 28.02, 28.07, 28.48, 28.6, 28.69, 28.56, 
            28.68, 28.5, 28.59, 28.54, 28.65, 29.2, 28.85, 28.31, 28.57, 
            28.73, 28.36, 28.04, 27.74, 27.72, 27.91, 27.55, 27.81, 27.78, 
            27.79, 28.06, 28.01, 28.02, 28.23, 28.56, 28.64, 28.62, 28.05, 
            28.01, 28.34, 28.57, 28.44, 28.9, 29.11, 29.06, 29.02, 29.24, 
            29.24, 29.32, 29.47, 29.43, 29, 28.92, 29.08, 28.7, 28.76, 28.76, 
            28.46, 28.31, 28.4, 28.28, 28.3, 28.65, 28.53, 28.48, 28.67, 
            29.01, 29.13, 28.98, 29.14, 29.3, 29.44, 29.75, 29.79, 29.92, 
            29.86, 29.49, 29.68, 29.47, 29.36, 29.48, 29.38, 29.41, 29.45, 
            29.79, 29.85, 29.88, 30.43, 30.35, 30.53, 30.45, 30.6, 30.69, 
            30.82, 30.78, 30.82, 30.94, 30.75, 30.79, 30.76, 30.75, 30.43, 
            30.35, 30.28, 30.21, 29.91, 30.15, 29.61, 29.82, 29.44, 29.63, 
            29.8, 29.68, 29.97, 29.88, 29.96, 29.83, 29.86, 29.67, 29.52, 
            29.22, 28.9, 28.77, 28.99, 28.85, 28.8, 28.99, 28.89, 28.93, 
            28.74, 28.84, 28.13, 28.33, 27.98, 27.83, 27.89, 28.07, 28, 27.99, 
            28.09, 28.05, 28.16, 28.4, 27.76, 28.3, 28.16, 27.92, 27.67, 
            27.55, 27.27, 27.58, 27.29, 27.4, 27.91, 27.74, 27.46, 27.43, 
            27.13, 27.25, 27.65, 27.53, 27.74, 27.94, 27.13, 26.87, 26.91, 
            26.47, 26.73, 26.31, 26.01, 25.28, 25.39, 25.16, 24.22, 24.07, 
            24.47, 24.63, 24.46, 24.6, 24, 24.46, 24.34, 24.13, 24.28, 24.64, 
            24.23, 24.18, 23.49, 23.09, 23.63, 23.35, 23.48, 23.11, 23.18, 
            22.97, 22.28, 22.71, 22.57, 22.2, 22.22, 22.62, 22.92, 22.64, 
            22.04, 21.94, 20.03, 20.6, 20.04, 20.04, 19.82, 19.51, 18.65, 
            18.86, 18.13, 17.61, 17.03, 16.24, 16.49, 16.52, 16.05, 17.03, 
            16.16, 16.68, 16.28, 16.06, 15.66, 15.65, 15.7, 15.28, 14.43, 
            13.82, 14.05, 14.2, 14, 13.27, 13.43, 13.99, 13.3, 14.04, 13.32, 
            13.57, 13.27, 12.94, 12.82, 13.11, 12.57, 12.65, 13.62, 14.27, 
            15.09, 14.09, 14.56, 14.76, 14.98), 
.indexTZ = "UTC", .indexCLASS = "Date", tclass = "Date", tzone = "UTC", src = "FRED", 
updated = structure(1423948051.10322, class = c("POSIXct", 
                                                "POSIXt")), 
index = structure(c(1388620800, 1388707200, 1388966400, 
                    1389052800, 1389139200, 1389225600, 1389312000, 1389571200, 1389657600, 
                    1389744000, 1389830400, 1389916800, 1390262400, 1390348800, 1390435200, 
                    1390521600, 1390780800, 1390867200, 1390953600, 1391040000, 1391126400, 
                    1391385600, 1391472000, 1391558400, 1391644800, 1391731200, 1391990400, 
                    1392076800, 1392163200, 1392249600, 1392336000, 1392681600, 1392768000, 
                    1392854400, 1392940800, 1393200000, 1393286400, 1393372800, 1393459200, 
                    1393545600, 1393804800, 1393891200, 1393977600, 1394064000, 1394150400, 
                    1394409600, 1394496000, 1394582400, 1394668800, 1394755200, 1395014400, 
                    1395100800, 1395187200, 1395273600, 1395360000, 1395619200, 1395705600, 
                    1395792000, 1395878400, 1395964800, 1396224000, 1396310400, 1396396800, 
                    1396483200, 1396569600, 1396828800, 1396915200, 1397001600, 1397088000, 
                    1397174400, 1397433600, 1397520000, 1397606400, 1397692800, 1398038400, 
                    1398124800, 1398211200, 1398297600, 1398384000, 1398643200, 1398729600, 
                    1398816000, 1398902400, 1398988800, 1399248000, 1399334400, 1399420800, 
                    1399507200, 1399593600, 1399852800, 1399939200, 1400025600, 1400112000, 
                    1400198400, 1400457600, 1400544000, 1400630400, 1400716800, 1400803200, 
                    1401148800, 1401235200, 1401321600, 1401408000, 1401667200, 1401753600, 
                    1401840000, 1401926400, 1402012800, 1402272000, 1402358400, 1402444800, 
                    1402531200, 1402617600, 1402876800, 1402963200, 1403049600, 1403136000, 
                    1403222400, 1403481600, 1403568000, 1403654400, 1403740800, 1403827200, 
                    1404086400, 1404172800, 1404259200, 1404345600, 1404691200, 1404777600, 
                    1404864000, 1404950400, 1405036800, 1405296000, 1405382400, 1405468800, 
                    1405555200, 1405641600, 1405900800, 1405987200, 1406073600, 1406160000, 
                    1406246400, 1406505600, 1406592000, 1406678400, 1406764800, 1406851200, 
                    1407110400, 1407196800, 1407283200, 1407369600, 1407456000, 1407715200, 
                    1407801600, 1407888000, 1407974400, 1408060800, 1408320000, 1408406400, 
                    1408492800, 1408579200, 1408665600, 1408924800, 1409011200, 1409097600, 
                    1409184000, 1409270400, 1409616000, 1409702400, 1409788800, 1409875200, 
                    1410134400, 1410220800, 1410307200, 1410393600, 1410480000, 1410739200, 
                    1410825600, 1410912000, 1410998400, 1411084800, 1411344000, 1411430400, 
                    1411516800, 1411603200, 1411689600, 1411948800, 1412035200, 1412121600, 
                    1412208000, 1412294400, 1412553600, 1412640000, 1412726400, 1412812800, 
                    1412899200, 1413158400, 1413244800, 1413331200, 1413417600, 1413504000, 
                    1413763200, 1413849600, 1413936000, 1414022400, 1414108800, 1414368000, 
                    1414454400, 1414540800, 1414627200, 1414713600, 1414972800, 1415059200, 
                    1415145600, 1415232000, 1415318400, 1415577600, 1415664000, 1415750400, 
                    1415836800, 1415923200, 1416182400, 1416268800, 1416355200, 1416441600, 
                    1416528000, 1416787200, 1416873600, 1416960000, 1417132800, 1417392000, 
                    1417478400, 1417564800, 1417651200, 1417737600, 1417996800, 1418083200, 
                    1418169600, 1418256000, 1418342400, 1418601600, 1418688000, 1418774400, 
                    1418860800, 1418947200, 1419206400, 1419292800, 1419379200, 1419552000, 
                    1419811200, 1419897600, 1419984000, 1420156800, 1420416000, 1420502400, 
                    1420588800, 1420675200, 1420761600, 1421020800, 1421107200, 1421193600, 
                    1421280000, 1421366400, 1421712000, 1421798400, 1421884800, 1421971200, 
                    1422230400, 1422316800, 1422403200, 1422489600, 1422576000, 1422835200, 
                    1422921600, 1423008000, 1423094400, 1423180800, 1423440000), 
                  tzone = "UTC", tclass = "Date"), .Dim = c(278L, 3L), 
.Dimnames = list(NULL, c("WTI", "BRENT", "DBO")), class = c("xts", "zoo"))

test_lr <- function (fast_only=FALSE) {
  test("partialCI:::loglik.pci.fkf(OIL$WTI, OIL$BRENT, 0, 1, -0.0195, 0.8483, 0.8364)", 447.8704)
  test("partialCI:::loglik.pci.ss(OIL$WTI, OIL$BRENT, 0, 1, -0.0195, 0.8483, 0.8364)", 443.1509)
  test("partialCI:::loglik.pci.css(OIL$WTI, OIL$BRENT, 0, 1, -0.0195, 0.8483, 0.8364)", 443.1509)
  test("partialCI:::loglik.pci.sst(OIL$WTI, OIL$BRENT, 0, 1, -0.0195, 0.8483, 0.8364)", 444.8827)
  test("partialCI:::loglik.pci.csst(OIL$WTI, OIL$BRENT, 0, 1, -0.0195, 0.8483, 0.8364)", 444.8827)
  
}

test.likelihood_ratio.pci <- function (fast_only=FALSE) {
  test("partialCI:::likelihood_ratio.pci(OIL$WTI, OIL$BRENT)", c(negloglik=-19.88135), tol=0.1)
  test("partialCI:::likelihood_ratio.pci(OIL$WTI, OIL$BRENT, null_model=\"ar1\")", c(negloglik=-18.71606), tol=0.1)
  test("partialCI:::likelihood_ratio.pci(OIL$WTI, OIL$BRENT, pci_opt_method=\"twostep\")", c(negloglik=-21.24474))
  test("partialCI:::likelihood_ratio.pci(OIL$WTI, OIL$BRENT, null_model=\"ar1\", pci_opt_method=\"twostep\")", c(negloglik=-19.97211))
  test("partialCI:::likelihood_ratio.pci(OIL$WTI, OIL$BRENT, robust=TRUE)", c(negloglik=-16.94564), tol=0.1)
  test("partialCI:::likelihood_ratio.pci(OIL$WTI, OIL$BRENT, robust=TRUE, null_model=\"ar1\")", c(negloglik=-16.71372), tol=0.1)
  test("partialCI:::likelihood_ratio.pci(OIL$WTI, OIL$BRENT, robust=TRUE, pci_opt_method=\"twostep\")", c(negloglik=-18.84097))
  test("partialCI:::likelihood_ratio.pci(OIL$WTI, OIL$BRENT, robust=TRUE, null_model=\"ar1\", pci_opt_method=\"twostep\")", c(negloglik=-16.87271))
}

test_lr2 <- function(fast_only=FALSE) {    
#    test.likelihood_ratio.par(fast_only)

    test("partialAR:::par.rw.pvalue(-3.5,400) < 0.05", TRUE)
    test("partialAR:::par.rw.pvalue(-1,500) > 0.10", TRUE)
    test("partialAR:::par.mr.pvalue(-1,600) < 0.05", TRUE)
    test("partialAR:::par.mr.pvalue(-0.1, 700) > 0.05", TRUE)
    test("partialAR:::par.rw.pvalue(-3.5,400, robust=TRUE) < 0.05", TRUE)
    test("partialAR:::par.rw.pvalue(-1,500, robust=TRUE) > 0.10", TRUE)
    test("partialAR:::par.mr.pvalue(-1,600, robust=TRUE) < 0.05", TRUE)
    test("partialAR:::par.mr.pvalue(-0.1, 700, robust=TRUE) > 0.05", TRUE)

    test("partialAR:::par.mr.pvalue(-2,400,ar1test='kpss') < 0.05", TRUE)
    test("partialAR:::par.mr.pvalue(-0.5, 500,ar1test='kpss') > 0.05", TRUE)
    test("partialAR:::par.mr.pvalue(-2,600, robust=TRUE,ar1test='kpss') < 0.05", TRUE)
    test("partialAR:::par.mr.pvalue(-0.5, 700, robust=TRUE,ar1test='kpss') > 0.05", TRUE)

    test("partialAR:::par.joint.pvalue(-4,-0.5,500) < 0.05", TRUE)
    test("partialAR:::par.joint.pvalue(-1,-0.25,500) > 0.05", TRUE)
    test("partialAR:::par.joint.pvalue(-5,-0.8,500, robust=TRUE) < 0.05", TRUE)
    test("partialAR:::par.joint.pvalue(-3,-0.1,500, robust=TRUE) > 0.05", TRUE)
    test("partialAR:::par.joint.pvalue(-5,-2,500, ar1test='kpss') < 0.05", TRUE)
    test("partialAR:::par.joint.pvalue(-3,-1,500, ar1test='kpss') > 0.05", TRUE)
    test("partialAR:::par.joint.pvalue(-4,-0.5,50000)", 0.03)
    test("partialAR:::par.joint.pvalue(-4,-0.5,50)", 0.10)
    test("partialAR:::par.joint.pvalue(4,-0.5,50)", 1)
    test("partialAR:::par.joint.pvalue(-4,-0.5,49)", 1)

    test("partialAR:::test.par.nullrw(data.L)$p.value < 0.05", TRUE)
    test("partialAR:::test.par.nullrw(data.IBM)$p.value > 0.05", TRUE)
    test("partialAR:::test.par.nullrw(data.L, robust=TRUE)$p.value < 0.10", TRUE)
    test("partialAR:::test.par.nullrw(data.IBM, robust=TRUE)$p.value > 0.10", TRUE)

    test("partialAR:::test.par.nullmr(data.L)$p.value <= 0.01", TRUE)
    test("partialAR:::test.par.nullmr(data.L, robust=TRUE)$p.value <= 0.01", TRUE)
    test("partialAR:::test.par.nullmr(data.L, ar1test='kpss')$p.value <= 0.01", TRUE)
    test("partialAR:::test.par.nullmr(data.L, robust=TRUE, ar1test='kpss')$p.value <= 0.01", TRUE)
    
    test("partialAR:::test.par.nullmr(data.IBM)$p.value < 0.05", TRUE)
    test("partialAR:::test.par.nullmr(data.IBM, robust=TRUE)$p.value < 0.10", TRUE)
    test("partialAR:::test.par.nullmr(data.IBM, ar1test='kpss')$p.value > 0.10", TRUE)
    test("partialAR:::test.par.nullmr(data.IBM, ar1test='kpss', robust=TRUE)$p.value > 0.10", TRUE)

    test("partialAR:::test.par(data.L, null_hyp='rw')$p.value == partialAR:::test.par.nullrw(data.L)$p.value", TRUE)
    test("partialAR:::test.par(data.IBM, null_hyp='rw')$p.value == partialAR:::test.par.nullrw(data.IBM)$p.value", TRUE)
    test("partialAR:::test.par(data.L, null_hyp='mr')$p.value == partialAR:::test.par.nullmr(data.L)$p.value", TRUE)
    test("partialAR:::test.par(data.IBM, null_hyp='mr')$p.value == partialAR:::test.par.nullmr(data.IBM)$p.value", TRUE)
    
    test("partialAR:::test.par(data.L)$p.value['PAR'] <= 0.01", c(PAR=TRUE))
    test("partialAR:::test.par(data.L, robust=TRUE)$p.value['PAR'] <= 0.10", c(PAR=TRUE))
    test("partialAR:::test.par(data.IBM)$p.value['PAR'] > 0.10", c(PAR=TRUE))
    test("partialAR:::test.par(data.IBM, robust=TRUE)$p.value['PAR'] > 0.10", c(PAR=TRUE))
    test("partialAR:::test.par(data.L, ar1test='kpss')$p.value['PAR'] <= 0.01", c(PAR=TRUE))
    test("partialAR:::test.par(data.L, ar1test='kpss',robust=TRUE)$p.value['PAR'] <= 0.10", c(PAR=TRUE))
    test("partialAR:::test.par(data.IBM, ar1test='kpss')$p.value['PAR'] > 0.10", c(PAR=TRUE))
    
    print(partialAR:::test.par(data.L))
    print(partialAR:::test.par(data.L, robust=TRUE))
    
    test("partialAR:::which.hypothesis.partest(partialAR:::test.par(data.L))", "PAR")
    test("partialAR:::which.hypothesis.partest(partialAR:::test.par(data.L, robust=TRUE))", "RRW")
    test("partialAR:::which.hypothesis.partest(partialAR:::test.par(data.IBM))", "RW")
    
    partialAR:::print.par.lrt(); cat("\n\n")
    partialAR:::print.par.lrt(robust=TRUE); cat("\n\n")
    partialAR:::print.par.lrt(latex=TRUE); cat("\n\n")
    
#    partialAR:::print.par.lrt.mr(); cat("\n\n")
#    partialAR:::print.par.lrt.mr(robust=TRUE); cat("\n\n")
#    partialAR:::print.par.lrt.mr(latex=TRUE); cat("\n\n")

    partialAR:::print.par.lrt.rw(); cat("\n\n")
    partialAR:::print.par.lrt.rw(robust=TRUE); cat("\n\n")
    partialAR:::print.par.lrt.rw(latex=TRUE); cat("\n\n")
    
}

test_fit_twostep <- function (fast_only = FALSE) {
    F <- partialCI:::fit.pci.twostep(OIL$WTI, OIL$BRENT, include_alpha=TRUE)
    test("F$alpha", c(alpha=3.988743))
    test("F$beta", c(beta_BRENT=0.9014), tol=0.01)
    test("F$rho", c(rho=-0.002161227))
    test("F$sigma_M", c(sigma_M=0.8509465))
    test("F$sigma_R", c(sigma_R=0.7937507))
    test("F$alpha.se", c(alpha.se=0.7837437))
    test("F$rho.se", c(rho.se=0.1696716))
    test("F$sigma_M.se", c(sigma_M.se=0.1056687))
    test("F$sigma_R.se", c(sigma_R.se=0.111671))
    test("F$negloglik", c(negloglik=436.6048))
    test("F$pvmr", c(pvmr=0.6972993))

    F0 <- partialCI:::fit.pci.twostep(OIL$WTI, OIL$BRENT)
    test("F0$beta", c(beta_BRENT=0.9014), tol=0.01)
    test("F0$rho", c(rho=-0.002161227))
    test("F0$sigma_M", c(sigma_M=0.8509465))
    test("F0$sigma_R", c(sigma_R=0.7937507))
    test("F0$rho.se", c(rho.se=0.1696716))
    test("F0$sigma_M.se", c(sigma_M.se=0.1056687))
    test("F0$sigma_R.se", c(sigma_R.se=0.111671))
    test("F0$negloglik", c(negloglik=436.6048))
    test("F0$pvmr", c(pvmr=0.6972993))

    FR0 <- partialCI:::fit.pci.twostep(OIL$WTI, OIL$BRENT, robust=TRUE)
    test("FR0$beta", c(beta_BRENT=0.911297), tol=0.01)
    test("FR0$rho", c(rho=-0.006326975))
    test("FR0$sigma_M", c(sigma_M=0.6299893))
    test("FR0$sigma_R", c(sigma_R=0.6845977))
    test("FR0$rho.se", c(rho.se=0.180226))
    test("FR0$sigma_M.se", c(sigma_M.se=0.09403069))
    test("FR0$sigma_R.se", c(sigma_R.se=0.08726811))
    test("FR0$negloglik", c(negloglik=429.9065))
    test("FR0$pvmr", c(pvmr=0.6302378))
    
    FR <- partialCI:::fit.pci.twostep(OIL$WTI, OIL$BRENT, robust=TRUE, include_alpha=TRUE)
    test("FR$alpha", c(alpha=3.344333))
    test("FR$beta", c(beta_BRENT=0.911297), tol=0.01)
    test("FR$rho", c(rho=-0.006326975))
    test("FR$sigma_M", c(sigma_M=0.6299893))
    test("FR$sigma_R", c(sigma_R=0.6845977))
    test("FR$alpha.se", c(alpha.se=0.6301423))
    test("FR$rho.se", c(rho.se=0.180226))
    test("FR$sigma_M.se", c(sigma_M.se=0.09403069))
    test("FR$sigma_R.se", c(sigma_R.se=0.08726811))
    test("FR$negloglik", c(negloglik=429.9065))
    test("FR$pvmr", c(pvmr=0.6302378))

}

test_fit_jointpenalty <- function (fast_only = FALSE) {
  guess <- c(alpha = 22.415066, beta_BRENT = 0.67375332, rho=-0.04976431, 
             sigma_M=0.78760586, sigma_R=0.83733729, M0=0, R0=0)  
  test("partialCI:::pci.jointpenalty.guess(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), include_alpha=TRUE)", guess)

  guess0 <- structure(c(0.673753323158499, -0.0499696397350701, 0.787500882009433, 
    0.837430798633094, 0, 22.4150662982716), .Names = c("beta_BRENT", 
      "rho", "sigma_M", "sigma_R", "M0", "R0"))
  test("partialCI:::pci.jointpenalty.guess(as.zoo(OIL$WTI), as.zoo(OIL$BRENT))", guess0)
 
  rw.par <- structure(c(22.4150662982132, 0.673753316670566, 0, 0, 1.23155657754959, 0, 0), 
                      .Names = c("alpha", "beta_BRENT", "rho", "sigma_M", "sigma_R", "M0", "R0"))
  test("partialCI:::fit.pci.jointpenalty.rw(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), include_alpha=TRUE)$par", rw.par)  

  rw.par0 <- structure(c(0.673753316670566, 0, 0, 1.23155657754959, 0, 22.415066298274), 
                      .Names = c("beta_BRENT", "rho", "sigma_M", "sigma_R", "M0", "R0"))
  test("partialCI:::fit.pci.jointpenalty.rw(as.zoo(OIL$WTI), as.zoo(OIL$BRENT))$par", rw.par0) 

  mr.par <- structure(c(19.0448876678824, 0.707408539377214, 0.979236271148003, 
                        1.22640513769963, 0, 0, 0), 
                      .Names = c("alpha", "beta_BRENT", 
                                 "rho", "sigma_M", "sigma_R", "M0", "R0"))
  test("partialCI:::fit.pci.jointpenalty.mr(as.zoo(OIL$WTI), as.zoo(OIL$BRENT),include_alpha=TRUE)$par", mr.par)  

  mr.par0 <- structure(c(0.704082607377607, 0.98035806173428, 1.22650034762294, 
    0, 0, 19.1413233596611), .Names = c("beta_BRENT", "rho", "sigma_M", 
      "sigma_R", "M0", "R0"))
   test("partialCI:::fit.pci.jointpenalty.mr(as.zoo(OIL$WTI), as.zoo(OIL$BRENT))$par", mr.par0)  

  both.par <- structure(c(14.5845792002844, 0.743252984476208, 0, 0.826758135981716, 
                          0.794395597894045, 0, 0), 
                        .Names = c("alpha", "beta_BRENT", "rho", 
                                   "sigma_M", "sigma_R", "M0", "R0"))

  test("partialCI:::fit.pci.jointpenalty.both(as.zoo(OIL$WTI), as.zoo(OIL$BRENT),include_alpha=TRUE)$par", both.par, tol=0.05)  

  both.par0 <- structure(c(0.743252984476208, 0, 0.826758135981716, 
                          0.794395597894045, 0, 14.5845792002844), 
                        .Names = c("beta_BRENT", "rho", 
                                   "sigma_M", "sigma_R", "M0", "R0"))

  test("partialCI:::fit.pci.jointpenalty.both(as.zoo(OIL$WTI), as.zoo(OIL$BRENT))$par", both.par0, tol=0.05)  

  rwrob.par <- structure(c(28.1949715154527, 0.620210939294621, 0, 0, 0.948726114365134, 
                           0, 0), .Names = c("alpha", "beta_BRENT", "rho", "sigma_M", "sigma_R", 
                                             "M0", "R0"))
  test("partialCI:::fit.pci.jointpenalty.rw(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), robust=TRUE, include_alpha=TRUE)$par", rwrob.par)

  rwrob.par0 <- structure(c(0.620210939294621, 0, 0, 0.948726114365134, 
                           0, 28.1949715154527), .Names = c("beta_BRENT", "rho", "sigma_M", "sigma_R", 
                                             "M0", "R0"))
  test("partialCI:::fit.pci.jointpenalty.rw(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), robust=TRUE)$par", rwrob.par0)
  
  mrrob.par <- structure(c(28.1948554986119, 0.62081912655694, 0.993012863223067, 
                           0.948691236006445, 0, 0, 0), .Names = c("alpha", "beta_BRENT", 
                                                                   "rho", "sigma_M", "sigma_R", "M0", "R0"))
  test("partialCI:::fit.pci.jointpenalty.mr(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), robust=TRUE, include_alpha=TRUE)$par", mrrob.par)
  
  mrrob.par0 <- structure(c(0.634017098277415, 0.992411921549702, 0.948914138555383, 
    0, 0, 26.7041944119358), .Names = c("beta_BRENT", "rho", "sigma_M", 
      "sigma_R", "M0", "R0"))
  test("partialCI:::fit.pci.jointpenalty.mr(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), robust=TRUE)$par", mrrob.par0)

  bothrob.par <- structure(c(20.098585528449, 0.692830550628575, 0, 0.599736912154181, 
                             0.67441954256228, 0, 0), .Names = c("alpha", "beta_BRENT", "rho", 
                                                                 "sigma_M", "sigma_R", "M0", "R0"))
  test("partialCI:::fit.pci.jointpenalty.both(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), robust=TRUE, include_alpha=TRUE)$par", bothrob.par, tol=0.05) 

  bothrob.par0 <- structure(c(0.692830550628575, 0, 0.599736912154181, 
                             0.67441954256228, 0, 20.098585528449), .Names = c("beta_BRENT", "rho", 
                                                                 "sigma_M", "sigma_R", "M0", "R0"))
  test("partialCI:::fit.pci.jointpenalty.both(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), robust=TRUE)$par", bothrob.par0, tol=0.05) 
  
}

test_fit <- function (fast_only = FALSE) {
  test_fit_twostep(fast_only)  
  test_fit_jointpenalty(fast_only)
  
  test("partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT))$negloglik", c(negloglik=432.4849))
  test("partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), robust=TRUE)$negloglik", c(negloglik=422.9159))
  test("partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), \"twostep\")$negloglik", c(negloglik=436.6048))
  test("partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), \"twostep\", robust=TRUE)$negloglik", c(negloglik=429.9065))
  test("partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), par_model=\"ar1\")$negloglik", c(negloglik=451.201))
  test("partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), par_model=\"rw\")$negloglik", c(negloglik=452.3663))
  test("partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), par_model=\"ar1\", robust=TRUE)$negloglik", c(negloglik=439.6296))
  test("partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), par_model=\"rw\", robust=TRUE)$negloglik", c(negloglik=439.8615))
  
  f <- partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), include_alpha=TRUE)
  last_state <- structure(list(Y = 52.99, Yhat = 57.0022715118106, Z = -4.01227151181061, 
                               M = 0.359169355426614, R = -4.37144086723722, eps_M = 0.369402426645854, 
                               eps_R = 0.579553335373364), .Names = c("Y", "Yhat", "Z", 
                                                                      "M", "R", "eps_M", "eps_R"), row.names = "2015-02-09", class = "data.frame")
  test("tail(partialCI:::statehistory.pci(partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), include_alpha=TRUE)),1)", last_state, tol=0.05)

   f0 <- partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT))
  last_state0 <- structure(list(Y = 52.99, Yhat = 42.3231449335717, Z = 10.6668550664283, 
    M = 0.355883860455669, R = 10.3109712059726, eps_M = 0.368030242920157, 
    eps_R = 0.58348828109853), .Names = c("Y", "Yhat", "Z", "M", 
     "R", "eps_M", "eps_R"), row.names = "2015-02-09", class = "data.frame")
    
  test("tail(partialCI:::statehistory.pci(partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT),)),1)", last_state0, tol=0.05)

 

  df <- structure(list(alpha = 14.6829401164186, beta = 0.742455481013507, 
    rho = -0.0232576651129888, sigma_M = 0.816415189337928, sigma_R = 0.804964566979587, 
    M0 = 0, R0 = 0, alpha.se = 6.1306344674343, beta.se = 0.0559775667569459, 
    rho.se = 0.172275543552826, sigma_M.se = 0.108139711899864, 
    sigma_R.se = 0.109367951001435, M0.se = NA_real_, R0.se = NA_real_, 
    negloglik = 432.475807192823, pvmr = 0.67807277837379), .Names = c("alpha", 
"beta", "rho", "sigma_M", "sigma_R", "M0", "R0", "alpha.se", 
"beta.se", "rho.se", "sigma_M.se", "sigma_R.se", "M0.se", "R0.se", 
"negloglik", "pvmr"), row.names = "beta_BRENT", class = "data.frame")
    
  test("as.data.frame(partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT), include_alpha=TRUE))", df, tol=0.05)

  df0 <- structure(list(beta = 0.742511314624065, rho = -0.0275437658638479, 
    sigma_M = 0.813095055217725, sigma_R = 0.808578327606414, 
    M0 = 0, R0 = 14.9933286994785, beta.se = 0.0560546241014983, 
    rho.se = 0.17155905646127, sigma_M.se = 0.107698809017417, 
    sigma_R.se = 0.108177642504822, M0.se = NA_real_, R0.se = NA_real_, 
    negloglik = 432.520303570585, pvmr = 0.675291879436992), .Names = c("beta", 
      "rho", "sigma_M", "sigma_R", "M0", "R0", "beta.se", "rho.se", 
      "sigma_M.se", "sigma_R.se", "M0.se", "R0.se", "negloglik", "pvmr"
    ), row.names = "beta_BRENT", class = "data.frame")
  test("as.data.frame(partialCI:::fit.pci(as.zoo(OIL$WTI), as.zoo(OIL$BRENT)))", df0, tol=0.05)
}

test_pci <- function (fast_only=FALSE) {
    # Comprehensive unit testing for PAR package

    options(warn=1)
    library(zoo)
    
    test_lr(fast_only)
    test.likelihood_ratio.pci(fast_only)
    test_fit(fast_only)
#    test_lr2(fast_only)
    
    if (all.tests.pass) {
        cat("SUCCESS! All tests passed.\n")
    } else {
        stop("ERRORS! ", all.tests.error.count," tests failed\n")
    }
}

test_pci(TRUE)
