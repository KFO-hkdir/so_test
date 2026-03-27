library(jsonlite)
library(glue)

source("script/helpers/chart_json_spec.R")
source("script/helpers/render_highcharts_preview_html.R")

line_chart_presentation <- build_chart_presentation(
  y_min = 0,
  tooltip_mode = "shared-number"
)

bar_chart_presentation <- build_chart_presentation(
  y_min = 0,
  tooltip_mode = "shared-number"
)

build_line_item <- function(label, categories, series_pairs) {
  build_chart_item(
    label = label,
    categories = categories,
    series = lapply(series_pairs, function(series_pair) {
      build_series_spec(series_pair$name, series_pair$data)
    })
  )
}

build_bar_item <- function(label, categories, series_name, values) {
  build_chart_item(
    label = label,
    categories = categories,
    series = list(build_series_spec(series_name, values))
  )
}

write_chart_pair <- function(output_path, chart_spec, dropdown_id = "chart-select") {
  json_path <- sub("\\.html$", ".chart.json", output_path)
  write_chart_json(chart_spec, json_path)
  write_preview_html_from_spec(chart_spec, output_path, dropdown_id = dropdown_id)
}

write_chart_pair(
  "figurer/side_1_utdanningsomr_hc/linje/nasjonalt_linje_test.html",
  build_single_chart_spec(
    chart_type = "line",
    categories = 2015:2025,
    series = list(
      build_series_spec("Søkere", c(127929, 132021, 135587, 142004, 138732, 150784, 154088, 134954, 135980, 142416, 142004)),
      build_series_spec("Studieplasser", c(53195, 54534, 55470, 57402, 58530, 58513, 61673, 62191, 62590, 64086, 64542))
    ),
    presentation = line_chart_presentation
  )
)

write_chart_pair(
  "figurer/side_1_utdanningsomr_hc/linje/utdomr_linje_dropdown.html",
  build_tabbed_chart_spec(
    chart_type = "line",
    control_label = "Utdanningsområde",
    items = list(
      build_line_item("Estetiske fag", 2015:2025, list(
        list(name = "Søkere", data = c(2657, 2607, 2811, 3106, 3019, 3411, 3342, 3003, 2987, 2938, 2752)),
        list(name = "Studieplasser", data = c(1368, 1385, 1407, 1470, 1496, 1507, 1540, 1589, 1616, 1628, 1616))
      )),
      build_line_item("Helsefag", 2015:2025, list(
        list(name = "Søkere", data = c(31963, 36258, 38095, 39572, 37703, 39254, 39389, 32850, 30380, 32177, 35245)),
        list(name = "Studieplasser", data = c(9899, 10203, 10350, 10335, 10600, 10821, 11441, 11531, 11779, 12124, 12426))
      )),
      build_line_item("Historiefag", 2015:2025, list(
        list(name = "Søkere", data = c(3520, 3245, 3549, 3720, 3678, 4504, 4393, 3818, 3626, 4104, 3762)),
        list(name = "Studieplasser", data = c(2940, 2879, 3036, 3067, 3132, 3214, 3346, 3336, 3442, 3196, 3071))
      )),
      build_line_item("Idrettsfag", 2015:2025, list(
        list(name = "Søkere", data = c(3514, 3027, 3421, 3805, 3584, 3615, 3235, 2895, 2915, 2920, 2834)),
        list(name = "Studieplasser", data = c(1522, 1541, 1509, 1543, 1578, 1498, 1519, 1439, 1593, 1670, 1607))
      )),
      build_line_item("Informasjonsteknologi", 2015:2025, list(
        list(name = "Søkere", data = c(3602, 3795, 4915, 5952, 6259, 7095, 7210, 6765, 7232, 7199, 5598)),
        list(name = "Studieplasser", data = c(1588, 1702, 1838, 2299, 2249, 2282, 2612, 2815, 2714, 2961, 3108))
      )),
      build_line_item("Juridiske fag", 2015:2025, list(
        list(name = "Søkere", data = c(9641, 9647, 9028, 8583, 8057, 7943, 10915, 9125, 10104, 9982, 10146)),
        list(name = "Studieplasser", data = c(2085, 2105, 2073, 2100, 1930, 1805, 1913, 1907, 1994, 2011, 2150))
      )),
      build_line_item("Land- og havbruk", 2015:2025, list(
        list(name = "Søkere", data = c(613, 733, 880, 878, 944, 1118, 1193, 1071, 1047, 1048, 1095)),
        list(name = "Studieplasser", data = c(345, 380, 428, 485, 519, 498, 539, 624, 599, 594, 900))
      )),
      build_line_item("Lærerutdanninger", 2015:2025, list(
        list(name = "Søkere", data = c(11916, 12129, 12350, 13987, 14122, 12974, 12421, 11005, 8600, 8235, 8029)),
        list(name = "Studieplasser", data = c(7244, 7112, 7238, 7338, 7889, 7792, 8122, 8037, 8044, 7844, 7384))
      )),
      build_line_item("Mediefag", 2015:2025, list(
        list(name = "Søkere", data = c(3535, 3465, 3425, 3593, 3667, 4561, 4871, 4131, 4264, 4345, 4026)),
        list(name = "Studieplasser", data = c(1791, 1825, 1765, 1755, 1455, 1511, 1625, 1596, 1614, 1662, 1587))
      )),
      build_line_item("Pedagogiske fag", 2015:2025, list(
        list(name = "Søkere", data = c(2330, 2550, 2661, 2845, 2931, 2933, 2926, 2228, 2361, 2366, 2223)),
        list(name = "Studieplasser", data = c(1190, 1275, 1313, 1318, 1393, 1333, 1401, 1381, 1393, 1443, 1571))
      )),
      build_line_item("Realfag", 2015:2025, list(
        list(name = "Søkere", data = c(4301, 4331, 4275, 4054, 3815, 4255, 4294, 3815, 3565, 3881, 3819)),
        list(name = "Studieplasser", data = c(2461, 2553, 2646, 2583, 2571, 2548, 2698, 2649, 2515, 2551, 2395))
      )),
      build_line_item("Reiselivsfag", 2015:2025, list(
        list(name = "Søkere", data = c(706, 682, 705, 567, 508, 458, 356, 352, 425, 333, 410)),
        list(name = "Studieplasser", data = c(400, 402, 362, 322, 317, 352, 372, 352, 301, 266, 230))
      )),
      build_line_item("Samfunnsfag", 2015:2025, list(
        list(name = "Søkere", data = c(14112, 14861, 15011, 15969, 15947, 19379, 17907, 15047, 15623, 17025, 16619)),
        list(name = "Studieplasser", data = c(5856, 6091, 6028, 6196, 6154, 6279, 6348, 6349, 6337, 6261, 6302))
      )),
      build_line_item("Språkfag", 2015:2025, list(
        list(name = "Søkere", data = c(5001, 5601, 5984, 5928, 5348, 5587, 5849, 4574, 4411, 4550, 4628)),
        list(name = "Studieplasser", data = c(3118, 3076, 3076, 3175, 3029, 3087, 3015, 3104, 3004, 3026, 2978))
      )),
      build_line_item("Teknologiske fag", 2015:2025, list(
        list(name = "Søkere", data = c(14048, 13535, 13330, 14059, 13900, 14628, 15313, 14496, 14999, 15484, 14710)),
        list(name = "Studieplasser", data = c(5705, 6101, 6340, 6793, 6657, 6322, 6685, 6661, 6477, 6846, 6954))
      )),
      build_line_item("Økonomisk-administrative fag", 2015:2025, list(
        list(name = "Søkere", data = c(16470, 15555, 15147, 15386, 15250, 19069, 20474, 19779, 23441, 25829, 26108)),
        list(name = "Studieplasser", data = c(5668, 5879, 6009, 6593, 7554, 7649, 8482, 8811, 9128, 10003, 10248))
      ))
    ),
    presentation = line_chart_presentation
  ),
  dropdown_id = "grp-select"
)

write_chart_pair(
  "figurer/side_1_utdanningsomr_hc/linje/utdomr_linje_dropdown_helsefag.html",
  build_tabbed_chart_spec(
    chart_type = "line",
    control_label = "Utdanningstype",
    items = list(
      build_line_item("Audio", 2015:2025, list(list(name = "Søkere", data = c(77, 75, 91, 87, 77, 66, 82, 70, 65, 98, 87)), list(name = "Studieplasser", data = rep(40, 11)))),
      build_line_item("Barnevern", 2015:2025, list(list(name = "Søkere", data = c(2214, 2500, 2470, 2693, 2687, 2782, 2386, 2014, 1706, 1782, 1689)), list(name = "Studieplasser", data = c(797, 788, 719, 749, 741, 771, 821, 841, 846, 855, 846)))),
      build_line_item("Bioingeniør", 2015:2025, list(list(name = "Søkere", data = c(725, 837, 908, 954, 921, 1064, 1101, 917, 913, 1005, 971)), list(name = "Studieplasser", data = c(254, 302, 299, 313, 323, 340, 353, 376, 393, 410, 417)))),
      build_line_item("Ergoterapi", 2015:2025, list(list(name = "Søkere", data = c(525, 535, 594, 657, 627, 696, 567, 536, 523, 513, 480)), list(name = "Studieplasser", data = c(294, 284, 284, 296, 312, 338, 307, 325, 322, 330, 302)))),
      build_line_item("Ernæring", 2015:2025, list(list(name = "Søkere", data = c(603, 651, 587, 531, 358, 398, 384, 686, 752, 789, 893)), list(name = "Studieplasser", data = c(196, 211, 220, 220, 173, 153, 159, 184, 189, 237, 357)))),
      build_line_item("Farmasi", 2015:2025, list(list(name = "Søkere", data = c(288, 286, 308, 268, 251, 228, 305, 244, 254, 230, 252)), list(name = "Studieplasser", data = c(92, 92, 92, 92, 92, 92, 107, 107, 92, 102, 102)))),
      build_line_item("Fysioterapi", 2015:2025, list(list(name = "Søkere", data = c(2063, 2089, 2032, 2175, 2244, 2259, 2200, 1923, 2055, 2365, 2341)), list(name = "Studieplasser", data = c(356, 376, 371, 382, 385, 362, 387, 385, 420, 460, 480)))),
      build_line_item("Medisin", 2015:2025, list(list(name = "Søkere", data = c(2955, 3168, 3337, 3608, 3520, 4107, 3940, 3989, 3493, 3706, 3639)), list(name = "Studieplasser", data = c(606, 636, 636, 636, 636, 636, 716, 731, 761, 821, 856)))),
      build_line_item("Odontologi", 2015:2025, list(list(name = "Søkere", data = c(744, 843, 861, 890, 884, 1071, 1095, 1041, 1061, 1173, 1139)), list(name = "Studieplasser", data = c(198, 198, 194, 214, 217, 215, 256, 257, 292, 312, 340)))),
      build_line_item("Optikk", 2015:2025, list(list(name = "Søkere", data = c(174, 176, 173, 184, 180, 200, 214, 181, 206, 233, 198)), list(name = "Studieplasser", data = c(54, 58, 58, 58, 60, 60, 60, 60, 60, 60, 60)))),
      build_line_item("Ortopediingeniør", 2015:2025, list(list(name = "Søkere", data = c(69, 77, 89, 93, 116, 115, 109, 105, 98, 114, 107)), list(name = "Studieplasser", data = c(20, 25, 25, 25, 25, 25, 20, 20, 20, 20, 20)))),
      build_line_item("Paramedisin", 2015:2025, list(list(name = "Søkere", data = c(0, 0, 0, 0, 0, 0, 756, 637, 618, 779, 760)), list(name = "Studieplasser", data = c(0, 0, 0, 0, 0, 0, 145, 170, 180, 197, 225)))),
      build_line_item("Radiografi", 2015:2025, list(list(name = "Søkere", data = c(478, 534, 537, 572, 561, 604, 573, 486, 521, 597, 587)), list(name = "Studieplasser", data = c(170, 170, 195, 198, 208, 218, 236, 251, 261, 281, 286)))),
      build_line_item("Reseptar", 2015:2025, list(list(name = "Søkere", data = c(177, 230, 260, 263, 255, 287, 336, 300, 350, 364, 337)), list(name = "Studieplasser", data = c(70, 70, 77, 77, 77, 77, 87, 87, 87, 97, 97)))),
      build_line_item("Sykepleie", 2015:2025, list(list(name = "Søkere", data = c(18099, 20653, 21849, 22489, 21427, 22558, 21889, 18396, 16572, 17186, 18882)), list(name = "Studieplasser", data = c(6770, 6810, 6908, 6817, 6983, 7112, 7515, 7492, 7464, 7456, 7440)))),
      build_line_item("Tannpleie", 2015:2025, list(list(name = "Søkere", data = c(699, 714, 668, 758, 781, 888, 852, 737, 651, 641, 620)), list(name = "Studieplasser", data = c(148, 152, 165, 165, 165, 170, 177, 177, 187, 177, 157)))),
      build_line_item("Vernepleie", 2015:2025, list(list(name = "Søkere", data = c(3104, 3553, 3867, 4253, 3966, 4279, 3995, 3422, 3284, 3787, 4044)), list(name = "Studieplasser", data = c(1110, 1121, 1091, 1127, 1081, 1112, 1177, 1117, 1165, 1235, 1302))))
    ),
    presentation = line_chart_presentation
  ),
  dropdown_id = "grp-select"
)

write_chart_pair(
  "figurer/side_1_utdanningsomr_hc/linje/utdomr_linje_dropdown_lærerutdanninger.html",
  build_tabbed_chart_spec(
    chart_type = "line",
    control_label = "Utdanningstype",
    items = list(
      build_line_item("GLU 5-10", 2015:2025, list(list(name = "Søkere", data = c(2510, 2583, 2440, 2746, 2633, 2294, 1999, 1925, 1245, 1099, 1260)), list(name = "Studieplasser", data = c(1399, 1429, 1445, 1613, 1675, 1634, 1642, 1642, 1614, 1560, 1509)))),
      build_line_item("GLU1-7", 2015:2025, list(list(name = "Søkere", data = c(2672, 2377, 2291, 2650, 2630, 2494, 2272, 2419, 1878, 1676, 1918)), list(name = "Studieplasser", data = c(1574, 1557, 1503, 1450, 1537, 1537, 1539, 1579, 1646, 1628, 1492)))),
      build_line_item("LUPE 1-13", 2015:2025, list(list(name = "Søkere", data = c(503, 610, 619, 617, 700, 724, 758, 594, 375, 395, 394)), list(name = "Studieplasser", data = c(322, 362, 317, 332, 317, 309, 322, 332, 347, 317, 307)))),
      build_line_item("Barnehage", 2015:2025, list(list(name = "Søkere", data = c(3215, 3330, 3589, 4251, 4542, 4212, 4116, 3061, 2557, 2557, 2153)), list(name = "Studieplasser", data = c(2627, 2458, 2604, 2531, 2702, 2690, 2864, 2730, 2773, 2695, 2505)))),
      build_line_item("Lektor", 2015:2025, list(list(name = "Søkere", data = c(1952, 2086, 2408, 2741, 2558, 2321, 2152, 1998, 1538, 1514, 1415)), list(name = "Studieplasser", data = c(936, 941, 999, 1093, 1188, 1187, 1305, 1284, 1219, 1189, 1092)))),
      build_line_item("Yrkesfaglærer", 2015:2025, list(list(name = "Søkere", data = c(435, 442, 366, 461, 458, 537, 565, 564, 482, 539, 453)), list(name = "Studieplasser", data = c(226, 205, 210, 199, 315, 315, 325, 335, 285, 315, 320))))
    ),
    presentation = line_chart_presentation
  ),
  dropdown_id = "grp-select"
)

write_chart_pair(
  "figurer/side_1_utdanningsomr_hc/linje/utdomr_linje_dropdown_teknologiske_fag.html",
  build_tabbed_chart_spec(
    chart_type = "line",
    control_label = "Utdanningstype",
    items = list(
      build_line_item("Arkitekt", 2015:2025, list(list(name = "Søkere", data = c(1233, 1224, 1320, 1293, 1374, 1498, 1672, 1488, 1550, 1231, 1021)), list(name = "Studieplasser", data = c(140, 140, 140, 140, 140, 140, 160, 160, 140, 144, 153)))),
      build_line_item("Ingeniør", 2015:2025, list(list(name = "Søkere", data = c(5762, 5567, 5151, 5247, 5101, 5439, 5541, 5373, 5185, 5923, 5876)), list(name = "Studieplasser", data = c(3067, 3390, 3423, 3763, 3691, 3388, 3451, 3428, 3467, 3573, 3600)))),
      build_line_item("Maritime", 2015:2025, list(list(name = "Søkere", data = c(461, 384, 448, 482, 631, 662, 882, 884, 1020, 983, 1097)), list(name = "Studieplasser", data = c(168, 158, 203, 218, 230, 238, 230, 230, 232, 245, 275)))),
      build_line_item("Sivilingeniør", 2015:2025, list(list(name = "Søkere", data = c(5325, 4892, 4912, 5269, 5377, 5622, 5739, 5262, 5820, 5948, 5683)), list(name = "Studieplasser", data = c(1807, 1853, 2074, 2096, 2115, 2100, 2378, 2317, 2191, 2431, 2433))))
    ),
    presentation = line_chart_presentation
  ),
  dropdown_id = "grp-select"
)

write_chart_pair(
  "figurer/side_1_utdanningsomr_hc/stolpe/utdomr_førsteperplass_bar.html",
  build_single_chart_spec(
    chart_type = "bar",
    categories = c("Juridiske fag", "Helsefag", "Samfunnsfag", "Mediefag", "Økonomisk-administrative fag", "Teknologiske fag", "Idrettsfag", "Informasjonsteknologi", "Reiselivsfag", "Estetiske fag", "Realfag", "Språkfag", "Pedagogiske fag", "Historiefag", "Land- og havbruk", "Lærerutdanninger"),
    series = list(build_series_spec("Førstevalg per plass", c(4.7, 2.8, 2.6, 2.5, 2.5, 2.1, 1.8, 1.8, 1.8, 1.7, 1.6, 1.6, 1.4, 1.2, 1.2, 1.1))),
    presentation = bar_chart_presentation
  )
)

write_chart_pair(
  "figurer/side_1_utdanningsomr_hc/stolpe/utdomr_bars_dropdown.html",
  build_tabbed_chart_spec(
    chart_type = "bar",
    control_label = "Vis:",
    items = list(
      build_bar_item("Planlagte studieplasser", c("Helsefag", "Økonomisk-administrative fag", "Lærerutdanninger", "Teknologiske fag", "Samfunnsfag", "Informasjonsteknologi", "Historiefag", "Språkfag", "Realfag", "Juridiske fag", "Estetiske fag", "Idrettsfag", "Mediefag", "Pedagogiske fag", "Land- og havbruk", "Reiselivsfag"), "Studieplasser", c(12426, 10248, 7389, 6954, 6312, 3108, 3071, 2978, 2395, 2150, 1616, 1607, 1587, 1571, 900, 230)),
      build_bar_item("Førstevalg", c("Helsefag", "Økonomisk-administrative fag", "Samfunnsfag", "Teknologiske fag", "Juridiske fag", "Lærerutdanninger", "Informasjonsteknologi", "Språkfag", "Mediefag", "Realfag", "Historiefag", "Idrettsfag", "Estetiske fag", "Pedagogiske fag", "Land- og havbruk", "Reiselivsfag"), "Førstevalg", c(35245, 26108, 16619, 14710, 10146, 8029, 5598, 4628, 4026, 3819, 3762, 2834, 2752, 2223, 1095, 410)),
      build_bar_item("Førstevalg per planlagte studieplass", c("Juridiske fag", "Helsefag", "Samfunnsfag", "Mediefag", "Økonomisk-administrative fag", "Teknologiske fag", "Idrettsfag", "Informasjonsteknologi", "Reiselivsfag", "Estetiske fag", "Realfag", "Språkfag", "Pedagogiske fag", "Historiefag", "Land- og havbruk", "Lærerutdanninger"), "Førstevalg per plass", c(4.7, 2.8, 2.6, 2.5, 2.5, 2.1, 1.8, 1.8, 1.8, 1.7, 1.6, 1.6, 1.4, 1.2, 1.2, 1.1))
    ),
    presentation = bar_chart_presentation
  ),
  dropdown_id = "chart-select"
)
