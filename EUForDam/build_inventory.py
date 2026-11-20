import sys
import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.datavalidation import DataValidation
from openpyxl.formatting.rule import FormulaRule

REW, OLD, OUT = sys.argv[1], sys.argv[2], sys.argv[3]

d = pd.read_excel(REW, sheet_name="EUForDam_REW")
d["v"] = pd.to_numeric(d.value1, errors="coerce")
d["NUTS"] = d["NUTS"].astype(str)
old_abio = pd.read_excel(OLD, sheet_name="Abiotic")

# ---------------------------------------------------------------- lookups
COUNTRY = {"AT": "Austria", "BG": "Bulgaria", "CH": "Switzerland", "CZ": "Czechia", "DE": "Germany",
           "FI": "Finland", "HR": "Croatia", "HU": "Hungary", "IT": "Italy", "LT": "Lithuania",
           "PL": "Poland", "RO": "Romania", "RS": "Serbia", "SE": "Sweden", "SI": "Slovenia", "SK": "Slovakia"}

# code -> (name, type). Names inferred from codes + host columns in EUForDam_REW -> verify.
AGENT = {
    "ips_typ": ("Ips typographus", "species"), "pit_cha": ("Pityogenes chalcographus", "species"),
    "ips_acu": ("Ips acuminatus", "species"), "ips_sex": ("Ips sexdentatus", "species"),
    "ips_dup": ("Ips duplicatus", "species"), "ips_cem": ("Ips cembrae", "species"),
    "ips_ami": ("Ips amitinus", "species"), "tom_spp": ("Tomicus piniperda & T. minor", "species"),
    "pha_cya": ("Phaenops cyanea", "species"), "pit_spp": ("Pityokteines spp. (fir)", "species"),
    "pit_bid": ("Pityogenes bidentatus", "species"), "pol_pol": ("Polygraphus poligraphus", "species"),
    "den_mic": ("Dendroctonus micans", "species"), "tet_gab": ("Tetropium gabrieli", "species"),
    "tet_spp": ("Tetropium spp.", "species"), "uro_sir_spp": ("Urocerus / Sirex spp.", "species"),
    "pis_pin": ("Pissodes pini / piniphilus", "species"), "sco_rat": ("Scolytus ratzeburgi", "species"),
    "sco_int": ("Scolytus intricatus", "species"), "sco_sco": ("Scolytus scolytus", "species"),
    "agr_big": ("Agrilus biguttatus", "species"), "agr_vir": ("Agrilus viridis", "species"),
    "sap_car": ("Saperda carcharias", "species"), "xyl_lin": ("Trypodendron (Xyloterus) lineatum", "species"),
    "xyl_spp": ("Xyleborus spp. (ambrosia beetles, oak)", "species"),
    "lym_mon": ("Lymantria monacha", "species"), "lym_dis": ("Lymantria dispar", "species"),
    "pan_fla": ("Panolis flammea", "species"), "dip_sp": ("Diprion spp.", "species"),
    "bup_pin": ("Bupalus piniaria", "species"), "den_pin": ("Dendrolimus pini", "species"),
    "tha_pit": ("Thaumetopoea pityocampa", "species"), "neo_ser": ("Neodiprion sertifer", "species"),
    "zei_gri": ("Zeiraphera griseana", "species"), "pri_abi": ("Pristiphora abietina", "species"),
    "cep_spp": ("Cephalcia spp.", "species"), "geo_db": ("Geometridae on oaks", "species group"),
    "tor_vir": ("Tortrix viridana", "species"), "era_def": ("Erannis defoliaria", "species"),
    "ope_spp": ("Operophtera spp.", "species"), "mel_spp": ("Melolontha spp.", "species"),
    "tha_pro": ("Thaumetopoea processionea", "species"), "clo_ana": ("Clostera anastomosis", "species"),
    "CambXyl_con&BB_con&SM": ("Spruce bark beetles – mixed (flagged as Ips typographus)", "group code"),
    "CambXyl_con&BB_con&BO": ("Bark beetles on pine – other/mixed", "group code"),
    "CambXyl_con&BO": ("Cambio-xylophages on conifers – other", "group code"),
    "CambXyl_con&MD": ("Cambio-xylophages on larch – mixed", "group code"),
    "Def_dec&DB": ("Defoliators on broadleaves – mixed", "group code"),
    "Def_dec": ("Defoliators on broadleaves (Aesculus)", "group code"),
}
host = d.drop_duplicates("sp_code").set_index("sp_code")[["HOSTTREE", "SP_CATEGORY"]]
unit_of = d.dropna(subset=["sp_unit"]).drop_duplicates("sp_code").set_index("sp_code")["sp_unit"]

UNIT_NAMES = {
    "AT11": "Burgenland", "AT12": "Niederösterreich", "AT13": "Wien", "AT21": "Kärnten", "AT22": "Steiermark",
    "AT31": "Oberösterreich", "AT32": "Salzburg", "AT33": "Tirol", "AT34": "Vorarlberg",
    "CZ01": "Praha", "CZ02": "Střední Čechy", "CZ03": "Jihozápad", "CZ04": "Severozápad", "CZ05": "Severovýchod",
    "CZ06": "Jihovýchod", "CZ07": "Střední Morava", "CZ08": "Moravskoslezsko",
    "DE1": "Baden-Württemberg", "DE2": "Bayern", "DE3": "Berlin", "DE4": "Brandenburg", "DE5": "Bremen",
    "DE6": "Hamburg", "DE7": "Hessen", "DE8": "Mecklenburg-Vorpommern", "DE9": "Niedersachsen",
    "DEA": "Nordrhein-Westfalen", "DEB; DEC": "Rheinland-Pfalz + Saarland", "DED": "Sachsen",
    "DEE": "Sachsen-Anhalt", "DEF": "Schleswig-Holstein", "DEG": "Thüringen",
    "FI196": "Satakunta", "FI198": "Keski-Suomi", "FI199": "Etelä-Pohjanmaa", "FI19A": "Pohjanmaa",
    "FI19B": "Pirkanmaa", "FI1B1": "Helsinki-Uusimaa", "FI1C1": "Varsinais-Suomi", "FI1C2": "Kanta-Häme",
    "FI1C5": "Etelä-Karjala", "FI1C6": "Päijät-Häme", "FI1C7": "Kymenlaakso", "FI1D5": "Keski-Pohjanmaa",
    "FI1D8": "Kainuu", "FI1D9": "Pohjois-Pohjanmaa", "FI1DA": "Etelä-Savo", "FI1DB": "Pohjois-Savo",
    "FI1DC": "Pohjois-Karjala", "FI1D7": "Lappi", "FI200": "Åland",
}
UNIT_NAMES.update(d[d.Country == "PL"].drop_duplicates("NUTS").set_index("NUTS")["Region"].to_dict())

# Target NUTS (codes valid in NUTS 2021 and 2024) for countries without sub-national data.
# Rule: NUTS 2; NUTS 3 where NUTS 2 has <=4 units (HR, LT, SI, SK) or the data cover one NUTS 2 (IT-FVG).
TARGET = {
    "BG": ("NUTS 2", [("BG31", "Severozapaden"), ("BG32", "Severen tsentralen"), ("BG33", "Severoiztochen"),
                      ("BG34", "Yugoiztochen"), ("BG41", "Yugozapaden"), ("BG42", "Yuzhen tsentralen")]),
    "HR": ("NUTS 3", [("HR021", "Bjelovarsko-bilogorska"), ("HR022", "Virovitičko-podravska"),
                      ("HR023", "Požeško-slavonska"), ("HR024", "Brodsko-posavska"), ("HR025", "Osječko-baranjska"),
                      ("HR026", "Vukovarsko-srijemska"), ("HR027", "Karlovačka"), ("HR028", "Sisačko-moslavačka"),
                      ("HR031", "Primorsko-goranska"), ("HR032", "Ličko-senjska"), ("HR033", "Zadarska"),
                      ("HR034", "Šibensko-kninska"), ("HR035", "Splitsko-dalmatinska"), ("HR036", "Istarska"),
                      ("HR037", "Dubrovačko-neretvanska"), ("HR050", "Grad Zagreb"), ("HR061", "Međimurska"),
                      ("HR062", "Varaždinska"), ("HR063", "Koprivničko-križevačka"), ("HR064", "Krapinsko-zagorska"),
                      ("HR065", "Zagrebačka")]),
    "HU": ("NUTS 2", [("HU11", "Budapest"), ("HU12", "Pest"), ("HU21", "Közép-Dunántúl"), ("HU22", "Nyugat-Dunántúl"),
                      ("HU23", "Dél-Dunántúl"), ("HU31", "Észak-Magyarország"), ("HU32", "Észak-Alföld"),
                      ("HU33", "Dél-Alföld")]),
    "IT": ("NUTS 3 (Friuli Venezia Giulia only)", [("ITH41", "Pordenone"), ("ITH42", "Udine"), ("ITH43", "Gorizia"),
                                                    ("ITH44", "Trieste")]),
    "LT": ("NUTS 3", [("LT011", "Vilniaus apskritis"), ("LT021", "Alytaus apskritis"), ("LT022", "Kauno apskritis"),
                      ("LT023", "Klaipėdos apskritis"), ("LT024", "Marijampolės apskritis"),
                      ("LT025", "Panevėžio apskritis"), ("LT026", "Šiaulių apskritis"), ("LT027", "Tauragės apskritis"),
                      ("LT028", "Telšių apskritis"), ("LT029", "Utenos apskritis")]),
    "RO": ("NUTS 2", [("RO11", "Nord-Vest"), ("RO12", "Centru"), ("RO21", "Nord-Est"), ("RO22", "Sud-Est"),
                      ("RO31", "Sud-Muntenia"), ("RO32", "București-Ilfov"), ("RO41", "Sud-Vest Oltenia"),
                      ("RO42", "Vest")]),
    "RS": ("NUTS 3 equivalent (oblasti; codes used in Serbian comments)", [("RS110", "Beogradska oblast"),
                      ("RS121", "Zapadnobačka"), ("RS122", "Južnobanatska"), ("RS123", "Južnobačka"), ("RS124", "Severnobanatska"),
                      ("RS125", "Severnobačka"), ("RS126", "Srednjebanatska"), ("RS127", "Sremska"), ("RS211", "Zlatiborska"),
                      ("RS212", "Kolubarska"), ("RS213", "Mačvanska"), ("RS214", "Moravička"), ("RS215", "Pomoravska"),
                      ("RS216", "Rasinska"), ("RS217", "Raška"), ("RS218", "Šumadijska"), ("RS221", "Borska"),
                      ("RS222", "Braničevska"), ("RS223", "Zaječarska"), ("RS224", "Jablanička"), ("RS225", "Nišavska"),
                      ("RS226", "Pirotska"), ("RS227", "Podunavska"), ("RS228", "Pčinjska"), ("RS229", "Toplička")]),
    "SE": ("NUTS 2", [("SE11", "Stockholm"), ("SE12", "Östra Mellansverige"), ("SE21", "Småland med öarna"),
                      ("SE22", "Sydsverige"), ("SE23", "Västsverige"), ("SE31", "Norra Mellansverige"),
                      ("SE32", "Mellersta Norrland"), ("SE33", "Övre Norrland")]),
    "SI": ("NUTS 3", [("SI031", "Pomurska"), ("SI032", "Podravska"), ("SI033", "Koroška"), ("SI034", "Savinjska"),
                      ("SI035", "Zasavska"), ("SI036", "Posavska"), ("SI037", "Jugovzhodna Slovenija"),
                      ("SI038", "Primorsko-notranjska"), ("SI041", "Osrednjeslovenska"), ("SI042", "Gorenjska"),
                      ("SI043", "Goriška"), ("SI044", "Obalno-kraška")]),
    "SK": ("NUTS 3", [("SK010", "Bratislavský kraj"), ("SK021", "Trnavský kraj"), ("SK022", "Trenčiansky kraj"),
                      ("SK023", "Nitriansky kraj"), ("SK031", "Žilinský kraj"), ("SK032", "Banskobystrický kraj"),
                      ("SK041", "Prešovský kraj"), ("SK042", "Košický kraj")]),
    "CH": ("NUTS 2", [("CH01", "Région lémanique"), ("CH02", "Espace Mittelland"), ("CH03", "Nordwestschweiz"),
                      ("CH04", "Zürich"), ("CH05", "Ostschweiz"), ("CH06", "Zentralschweiz"), ("CH07", "Ticino")]),
}

# Official names from Eurostat NUTS 2024 (Nuts2json, GISCO) – all codes used here were checked against it.
import json as _json, os as _os
_ES = _os.path.join(_os.path.dirname(_os.path.abspath(sys.argv[0])), "eurostat_nuts2024_names.json")
if _os.path.exists(_ES):
    _N = {k: v.strip() for k, v in _json.load(open(_ES, encoding="utf-8")).items()}
    for _c, (_lvl, _us) in TARGET.items():
        TARGET[_c] = (_lvl, [(u, _N.get(u, nm)) for u, nm in _us])
    for _u in list(UNIT_NAMES):
        if _u in _N:
            UNIT_NAMES[_u] = _N[_u]
    for _u in ("FI1D7", "FI200"):
        UNIT_NAMES[_u] = _N.get(_u, UNIT_NAMES.get(_u))

SUBNAT = ["AT", "CZ", "DE", "FI", "PL"]
LEVEL = {"AT": "NUTS 2", "CZ": "NUTS 2 (rows are NUTS 3 kraje, labelled with NUTS 2 code)",
         "DE": "NUTS 1 (RP + SL lumped)", "FI": "NUTS 3", "PL": "RDLP forest regions (treated as NUTS)"}
NUTS_STATUS = {"AT": "Yes", "CZ": "Yes", "FI": "Yes – 2 regions missing", "DE": "Yes – NUTS 1 only",
               "PL": "Yes"}
FLAGS = {
    "AT": "Series start 2002. Optional: ask whether 2000–2001 exist. Incoming has Ips cembrae (2002–2022) – not in EUForDam AT. L. dispar, Diprion and broadleaf defoliators in EUForDam are not in the Incoming file → other source.",
    "CZ": "Several rows per NUTS 2 × year × agent (= NUTS 3 kraje without their own code). Ask CZ for NUTS 3 codes so rows can be told apart. No 'ips_typ' code – Ips typographus is inside the spruce bark-beetle group code.",
    "DE": "EUForDam zeros are often MISSING data: 120 state-years with no Incoming value are stored as 0 for Ips typographus alone (e.g. Niedersachsen every year, NRW 2018). Values = Incoming × fixed factor per state (2.0–5.9, probably state forest → all forest) – confirm. Incoming ends 2019; source of 2020–2022 unknown. DEB and DEC are separate in Incoming in some years – split possible.",
    "FI": "Only Ips typographus (salvage-logging ha × 122). Series start 2012. Lapland (FI1D7) 'not included' and Åland (FI200) 'not available' per Incoming – ask whether Lapland can be added.",
    "PL": "Units are RDLP forest regions – treated the same as NUTS (decided): update 2023–2025 only. Series start 2003. RDLP→NUTS 3 key exists in Incoming if ever needed.",
    "BG": "2000–2004 blank (no data) for 7 of 8 agents; Tortrix viridana 2000–2004 stored as 0 (fake zeros). Incoming already has 2023 for Ips typographus, I. acuminatus, I. sexdentatus – do not re-ask.",
    "RO": "Ips typographus and Ips duplicatus end 2021.",
    "RS": "Ips typographus 2000–2002 blank ('missing data'). 'Ips typographus' = all bark beetles (90% I. typographus). Serbian comments use NUTS 3 codes (RS110, RS211–RS229) → target NUTS 3.",
    "SK": "All series end 2021. EUForDam 'Ips duplicatus' is really SK's 'Other bark beetles' column (mislabelled) – not used for presence marks. Pityokteines spinidens is in Incoming but not in EUForDam.",
    "SE": "Only Ips typographus. Zeros in non-outbreak years may be missing, not zero.",
    "CH": "Only Ips typographus.",
    "IT": "Only Friuli Venezia Giulia covered.",
    "LT": "17 agent series at national level. Conversion factors (Ips m3 × 2, Pityogenes ha × 40) undocumented.",
}
# ---------------------------------------------------------------- Incoming sheets (checked so far)
# abiotic inputs: (wind_in, wind_level, wind_y0, wind_y1, tot_in, tot_level, tot_y0, tot_y1, note)
ABIO = {
    "CZ": ("N", None, None, None, "Y", "National", 2000, 2021,
           "Abiotic total only (m3), national; no wind split. 2022 = '–'."),
    "SK": ("N", None, None, None, "Y", "National", 2000, 2021,
           "Abiotic total only (m3), national; no wind split. 2022 = '–'. Comments name windstorms (Alžbeta 2004, Filip 2007, Žofia 2014) and NUTS 3 codes."),
    "HU": ("Y", "National", 2000, 2022, "Y", "National", 2000, 2022,
           "'Storm' and abiotic total, both in ha (not m3). Also drought and late frost (ha). No NUTS breakdown in this file."),
    "RO": ("Y", "National", 2000, 2021, "N", None, None, None,
           "Wind-blown in ha (2000–2021) and m3 (2000–2010, 2015–2021; 2011–2014 'no data'). Abiotic total column empty. Also abnormal drying of fir/beech/oak (ha)."),
    "PL": ("Y", "NUTS", 2003, 2022, "N", None, None, None,
           "No labelled wind or abiotic series. 'Processed broken and fallen trees' (m3) per RDLP × spruce/pine/oak 2003–2022 entered as a wind/snow PROXY – confirm. Level = RDLP, same as insects."),
    "AT": ("N", None, None, None, "Y", "NUTS", 2002, 2022,
           "ABIO_Tot by NUTS 2 (m3), 2002–2022, all 9 units every year. 5 UNLABELLED rows per NUTS-year (2 in 2002) → sub-types (wind, snow …) lost. Wind may be one of them – ask AT for labelled sub-types."),
    "DE": ("N", None, None, None, "Y", "NUTS", 2000, 2019,
           "ABIO_tot by state (NUTS 1), 2000–2019, only 7–10 of 16 states per year, mixed m3/ha, 1–5 unlabelled rows per state-year. No wind code. (2007 IS present – v9 said missing.)"),
    "BG": ("N", None, None, None, "Y", "National", 2005, 2022,
           "Abiotic total in ha, national, 2005–2022 (same in both BG files). No wind split."),
    "RS": ("N", None, None, None, "Y", "National", 2000, 2022,
           "Abiotic total in m3, national, 2000–2022. No wind split; comments mention windthrow in Stara planina (RS226) and Kopaonik (RS216/217)."),
    "SE": ("Y", "National", 2001, 2022, "Y", "National", 2001, 2022,
           "Storm-felled volume only, million m3, 'very rough estimates', storm years only (gaps). Abiotic total = the same series. Comments name NUTS 2 regions."),
    "SI": ("N", None, None, None, "Y", "National", 2000, 2022,
           "Abiotic total m3 (windthrow, ice storm, landslides, snow, fire, pollution), no split. Comments name regions (ice storm 2014 all NUTS; windthrow 2017–2018 Koroška etc.)."),
    "IT": ("Y", "National", 2018, 2020, "Y", "National", 2000, 2022,
           "Friuli Venezia Giulia regional totals (ha + m3) 2000–2022 – one NUTS 2, not the NUTS 3 target. Wind = Vaia storm only (2018–2020, m3); 350,000 m3 Vaia noted as 'not in the database'."),
    "CH": ("Y", "National", 2000, 2022, "Y", "National", 2000, 2022,
           "CSV by category (m3): windthrow, snow/ice, avalanche, drought, fire, mass movement, other. File 'total' includes bark beetle + ash dieback → abiotic total must be summed. 2023 row is incomplete (bark beetle 0) – not counted."),
    "FI": ("Y", "National", 2009, 2022, "Y", "National", 2009, 2022,
           "NFI 'area affected' (ha) for wind, snow, total abiotic 2009–2022 (Melin file) – area, not volume, so not comparable to m3 elsewhere. Fire (ha) 1996–2022 from rescue services."),
    "HR": ("N", None, None, None, "Y", "National", 2000, 2022,
           "Abiotic total in ha AND m3, national, 2000–2022. No wind split (ice storm 2014 and storms mentioned in comments)."),
    "LT": ("Y", "National", 2000, 2022, "Y", "National", 2000, 2022,
           "State forests, scaled to all forest with an area coefficient (1.29 in 2000 → 1.93 in 2022). Wind damage (ha; m3 from 2001), wind + snow (m3), abiotic total (ha). Red cells = expert guesses. District data exist in the Sanitary Reviews."),
}
# insect-side check of each Incoming file
INCOMING = {
    "HR": ("AbiBioCro_1_corrMilan (final version; Milan Pernek)", "National; comments give NUTS 3 codes for every agent", "2000–2022", "No",
           "Geometridae, L. dispar, Ips typographus, Pityokteines spp., Scolytinae on pine, Thaumetopoea pityocampa, Corythucha arcuata, Stereonychus fraxini, Dryocosmus kuriphilus, T. processionea + fungi",
           "Stereonychus fraxini, Dryocosmus kuriphilus, Corythucha arcuata, Thaumetopoea processionea; fungi (Hymenoscyphus, Cryphonectria, Erysiphe, Phytophthora, Biscogniauxia)",
           "All 6 shared series match EUForDam 1:1. NUTS 3 codes in comments for every agent/year → good basis for SP_HR and a NUTS 3 resend."),
    "LT": ("EuropePests_2023_v5_Lithuania_3OK_1.xlsx (Gediminas Brazaitis)", "National (state forests, scaled)", "2000–2022", "No",
           "~30 agents incl. Ips typographus (ha + m3), Pityogenes, Polygraphus, Dendroctonus micans, Ips acuminatus, Tomicus, Pissodes, Dendrolimus, Diprion, Geometridae, L. dispar, L. monacha, Panolis, Phaenops, Scolytus spp., Tortrix + 'Rest of species' sheet",
           "Hylobius abietis, Melolontha, Microtus, fungi; 'Rest of species' (Physokermes piceae, Lygaeonematus abietinus, Aradus cinnamomeus …)",
           "EUForDam ips_typ = m3 column × 2 where m3 exists; years without m3 (2002, 2007–2014) come from elsewhere. Pityogenes = ha × 40. Undocumented factors – confirm. Values already scaled state → all forest. Red cells = expert guesses."),
    "SE": ("EuropePests_2023_v5_Sweden_new_1_1.xlsx (Martin Schroeder)", "National; comments name NUTS 2/3", "2006–2022, outbreak years only", "No",
           "Ips typographus in two columns (south / north), million m3",
           "Polygraphus poligraphus mentioned (2009 outbreak mix) – no values",
           "EUForDam = south + north × 1,000,000 (matches). File says 'no estimates of damages when not considered to be outbreak' → EUForDam zeros 2000–2005, 2012–2014, 2017 are MISSING, not 0."),
    "SI": ("EuropePests_2023_v5_SLO_1_1.xlsx (Maarten de Groot, Marija Kolšek)", "National; comments name regions", "2000–2022", "No",
           "Ips typographus, Pityokteines spp. (m3), Corythucha arcuata (number of locations)",
           "Corythucha arcuata (observations 2016–2022)",
           "Ips typographus matches EUForDam 2001–2013 exactly but EUForDam is 2–9% lower from 2014; EUForDam has a 2000 value where file is blank; Pityogenes chalcographus (2014+) and the 'conifers – other' group are not in this file → EUForDam SI partly from another source."),
    "IT": ("Friuli_Venezia_Giulia_1.xlsx", "Friuli Venezia Giulia region (= NUTS 2 ITH4)", "2000–2022", "No",
           "Summary sheet: biotic total, Ips typographus (m3); breakdown sheet: Ips typographus, Pityogenes, Ips sexdentatus, I. acuminatus, Xyloterus, Hylesinus fraxini, fungi",
           "Hylesinus fraxini, Pissodes piceae, Xyleborus dispar, Epinotia tedella; fungi (Heterobasidion, Nectria …)",
           "EUForDam ips_typ = summary sheet (e.g. 2000 = 338). Breakdown sheet gives different Ips values (2000 = 209) – EUForDam Pityogenes comes from the breakdown sheet, so the two sheets are mixed. Only FVG covered."),
    "CH": ("damage_ch_2000-2023_1m3_2024_01_30_1.csv", "National", "2000–2023 (2023 incomplete)", "2023 row exists but bark beetle = 0 → treat as missing, still ask",
           "Bark beetle (= Ips typographus), ash dieback (2016+), 'insects' (all 0)",
           "Ash dieback (fungus)",
           "Bark beetle matches EUForDam 1:1 (2000–2022). Ash dieback 0 before 2016 = not recorded. Collected by forest districts, so NUTS aggregation should be feasible."),
    "FI": ("Data_Tiina_Ilioya_Feb24_loggings_Ipstypo_1.xlsx (Tiina Ylioja, Markus Melin, Luke) + Data_Markus_Melin_10Oct2023 (NFI)", "NUTS 3 (17 included); Lapland 'not included', Åland 'not available'", "2012–2022", "No",
           "Ips typographus ('mainly') – hectares of salvage loggings announced due to insect damage in spruce stands",
           "–",
           "EUForDam FI = ha × 122 every year (confirmed). Lapland and Åland absent by design (file says so) – ask whether Lapland can be added. Logged area > area with dead trees (file note)."),
    "AT": ("Austria_NUTS2_damage_m3_NS_1.xlsx (provider Gernot Hoch)", "NUTS 2 (9), long format, one sheet per year", "2002–2022", "No",
           "Ips typographus, Pityogenes chalcographus, Pityokteines, Tomicus, Phaenops, Tetropium gabrieli, pine bark beetles mix, Ips cembrae (m3)",
           "Ips cembrae (2002–2022)",
           "All 7 shared series match EUForDam 1:1. EUForDam AT L. dispar, Diprion and broadleaf defoliators (ha) are NOT in this file → other source. Abiotic sub-types unlabelled."),
    "DE": ("Germany_clean_NS_1.xlsx", "NUTS 1 (states), long format, one sheet per year; DEB/DEC sometimes separate", "2000–2019", "No",
           "~85 agent codes (insects, fungi, game, mistletoe) – volume (m3) or area (ha) per row",
           "Ips cembrae etc. are in EUForDam; not in EUForDam: fungi (Armillaria, Heterobasidion …), Hylobius, Melolontha pinetum? (mel_pin), Viscum album, game",
           "EUForDam DE ips_typ = Incoming × fixed factor per state (DE1 2.94, DE2 3.33, DE4 2.94, DE8 2.0, DEA 5.88, DED 3.33, DEE 3.33, DEF 5.0, DEG 2.5) – confirm method. 120 state-years absent from Incoming stored as 0 in EUForDam → fake zeros. Incoming ends 2019 but EUForDam runs to 2022."),
    "BG": ("EuropePests_2023_v5_BG_1.xlsx + BG_additional_defoliators_14_February.xlsx (G. Georgiev, M. Georgieva, FRI-BAS)", "National; comments give NUTS 2 (BG33, BG41, BG42)", "2005–2023 (bark beetles), 2005–2022 (defoliators)", "YES – 2023 for Ips typographus, I. acuminatus, I. sexdentatus (ha)",
           "Ips typographus, I. acuminatus, I. sexdentatus (ha); L. dispar, Thaumetopoea pityocampa, Neodiprion sertifer, Tortricidae, Geometridae (ha)",
           "–",
           "Bark beetles: EUForDam = ha × 286.4 (I. typographus) / × 100 (I. acuminatus, I. sexdentatus) – confirmed. Defoliators: EUForDam = ha × 1.818 – undocumented, confirm. Tortrix 2000–2004 = 0 in EUForDam but no data in Incoming. Bark-beetle data typed into the 'Example of Czechia' sheet."),
    "RS": ("EuropePests_2023_v5_Serbia_3_Febrary_2024_1.xlsx (Slobodan Milanović)", "National; comments give NUTS 3 (RS110, RS211–RS229)", "2000–2022", "No",
           "Bark beetles (90% Ips typographus, m3), Lymantria dispar (ha)",
           "–",
           "Both series match EUForDam 1:1. Bark beetles 2000–2002 'missing data'."),
    "CZ": ("EuropePests_2023_v5_CR_1.xlsx (filled by Roman Modlinger, CULS)", "National; comments give okres (NUTS 4/LAU) codes, key sheet included",
           "2000–2021 (2022 '–')", "No",
           "Ips typographus, Ips duplicatus, Ips acuminatus, Tomicus sp., Cephalcia, Tenthredinidae on spruce, defoliators on oaks",
           "Hylobius abietis, rodents, Lophodermium, Armillaria (not in scope?)",
           "EUForDam CZ is NUTS 2 with 21 agent codes to 2022, so it comes from another source (Roman?) – this national sheet is not the source."),
    "SK": ("EuropePests_2023_SK_v5_1.xlsx (Jozef Vakula, NLC Zvolen)", "National; comments name NUTS 3", "2000–2021 (2022 '–')", "No",
           "Ips typographus, Pityogenes chalcographus, pine bark beetles (80% I. acuminatus), Pityokteines spinidens, other bark beetles, Lymantria dispar",
           "Pityokteines spinidens (fir, 2007–2021); Armillaria, Chalara (pathogens)",
           "ERROR IN EUForDam: SK 'ips_dup' (Ips duplicatus) = the 'Other bark beetles' column, identical every year – mislabelled. 2007 blank stored as 0. Pine column stored 1:1 as Ips acuminatus although only 80%."),
    "HU": ("EuropePests_2023_Hungary_1.xlsx (Hirka, Csóka, Univ. Sopron)", "National", "2000–2022", "No",
           "Bark beetles (all), Lymantria dispar, Corythucha arcuata – all ha",
           "Corythucha arcuata (2018–2022, ha); game damage",
           "EUForDam HU ips_typ = 'Bark beetles' ha × 160 (checked, every year); L. dispar 1:1. Bark beetles are not only Ips typographus."),
    "RO": ("EuropePests_2023_v5_RO_Duduman_1.xlsx (Mihai-Leonard Duduman)", "National; comments name NUTS 2/3 (RO212–RO215)", "2000–2021 (2022 'no data')", "No",
           "Ips typographus (ha 2000–2021; two m3 columns 2015–2021), Ips duplicatus (m3 2015–2021)",
           "–",
           "EUForDam ips_typ = ha column × 1.52 (2000) rising to × 2.83 (2011+) – method unclear, confirm. EUForDam ips_dup 2000–2014 = 0 but file says 'no data' → fake zeros. RO L. dispar, Tortrix, Geometridae, Clostera in EUForDam are not in this file → other source."),
    "PL": ("POLAND_Clean_Oct23_1.xlsx", "RDLP (17) + RDLP→NUTS 3 key (72 NUTS 3)", "2003–2022", "No",
           "Ips typographus, Phaenops, Tomicus, Ips acuminatus, Agrilus biguttatus (processed infested / standing dead / broken & fallen / total, m3); defoliators table (ha)",
           "Acantholyda spp. (aca_spp), Viscum album (vis_alb)",
           "EUForDam PL ips_typ = Incoming × 1.25 (checked 2003–2008) – undocumented, confirm. RDLP→NUTS 3 key exists, so PL could be converted to NUTS 3."),
}

REQ_SUB = "UPDATE: add 2023, 2024, 2025 for every agent × unit series already held (3 values each – see Inventory_Series)"
REQ_NAT = "FULL SERIES: 2000–2025 at NUTS level for all agents (target units: Target_NUTS) – replaces the national series"
YEARS = list(range(2000, 2023))


def yr_ranges(ys):
    ys = sorted(ys)
    if not ys:
        return "–"
    out, s, p = [], ys[0], ys[0]
    for y in ys[1:]:
        if y == p + 1:
            p = y
            continue
        out.append(f"{s}" if s == p else f"{s}–{p}")
        s = p = y
    out.append(f"{s}" if s == p else f"{s}–{p}")
    return ", ".join(out)


# ---------------------------------------------------------------- series inventory
ser_rows, zero_rows = [], []
for (c, code), g in d.groupby(["Country", "sp_code"]):
    units = sorted(g.NUTS.unique())
    have = set(g.loc[g.v.notna(), "d_year"])
    y0, y1 = min(have), max(have)
    blank = [y for y in YEARS if y not in have]
    zero_units = [u for u in units if not (g.loc[g.NUTS == u, "v"].fillna(0) > 0).any()]
    for u in zero_units:
        zero_rows.append((c, COUNTRY[c], code, AGENT[code][0], u, UNIT_NAMES.get(u, u)))
    if c in SUBNAT:
        req = "2023–2025"
        opt = f"Optional: {yr_ranges([y for y in blank if y < y0])} if available" if y0 > 2000 else ""
    else:
        req = "2000–2025 at NUTS level"
        opt = ""
    ser_rows.append([c, COUNTRY[c], code, AGENT[code][0], AGENT[code][1], host.loc[code, "HOSTTREE"],
                     unit_of.get(code, ""), len(units), y0, y1, yr_ranges(blank),
                     int((g.v > 0).sum()), len(zero_units), req, opt])
ser = pd.DataFrame(ser_rows)

# ---------------------------------------------------------------- styles
F = "Arial"
hdr_font = Font(name=F, bold=True, color="FFFFFF")
hdr_fill = PatternFill("solid", fgColor="305496")
body = Font(name=F, size=10)
bold = Font(name=F, size=10, bold=True)
title = Font(name=F, size=14, bold=True)
red = PatternFill("solid", fgColor="F8CBAD")
green = PatternFill("solid", fgColor="C6EFCE")
amber = PatternFill("solid", fgColor="FFE699")
grey = PatternFill("solid", fgColor="BFBFBF")
lgrey = PatternFill("solid", fgColor="EDEDED")
yellow = PatternFill("solid", fgColor="FFF2CC")
thin = Side(style="thin", color="BFBFBF")
box = Border(left=thin, right=thin, top=thin, bottom=thin)
wrap = Alignment(wrap_text=True, vertical="top")
center = Alignment(horizontal="center", vertical="center", wrap_text=True)


def table(ws, r0, headers, rows, widths):
    for j, h in enumerate(headers, 1):
        c = ws.cell(r0, j, h)
        c.font, c.fill, c.alignment, c.border = hdr_font, hdr_fill, center, box
    for i, row in enumerate(rows, r0 + 1):
        for j, val in enumerate(row, 1):
            c = ws.cell(i, j, val)
            c.font, c.alignment, c.border = body, wrap, box
    for j, w in enumerate(widths, 1):
        ws.column_dimensions[get_column_letter(j)].width = w
    ws.freeze_panes = ws.cell(r0 + 1, 3)
    ws.auto_filter.ref = f"A{r0}:{get_column_letter(len(headers))}{r0 + len(rows)}"


wb = Workbook()

# ---------------------------------------------------------------- README
ws = wb.active
ws.title = "README"
lines = [
    ("EUForDam – data inventory & data request (v10)", title),
    ("Built from EUForDam_REW.xlsx (sheet EUForDam_REW, 2000–2022, 49 agent codes, 16 countries). "
     "Replaces v9, which was built from an older 9-species file.", body),
    ("", body),
    ("Request rules (from the professor's brief)", bold),
    ("1. Countries WITH NUTS-level data → add 2023, 2024, 2025 only (three values per agent × NUTS).", body),
    ("2. Countries WITHOUT NUTS-level data → full series 2000–2025 at NUTS level (replaces the national series).", body),
    ("3. Abiotic (wind + total abiotic): same logic, based on what is in the Incoming sheets. Fill the yellow cells in 'Abiotic'; the request columns update themselves.", body),
    ("Items in 'optional / to clarify' columns go beyond the brief – decide before sending.", body),
    ("", body),
    ("Sheets", bold),
    ("Inventory_Countries – one row per country: what we hold, NUTS yes/no, request for insects and abiotic.", body),
    ("Inventory_Series – every agent series per country: units, years held, blanks, request.", body),
    ("Abiotic – checklist to fill from the Incoming sheets (yellow = input).", body),
    ("Zero_Only_Series – series that exist but are 0 in every year for a unit (true absence or not monitored?).", body),
    ("Agent_Codes – code → name used here. Names are inferred from the codes and host columns – please verify.", body),
    ("Species_List – which species go into the expert presence sheets and why.", body),
    ("Target_NUTS – NUTS units for each country (data units for countries with sub-national data; NUTS 2021 targets for the rest).", body),
    ("SP_xx – species × NUTS presence sheet per country, for the experts.", body),
    ("", body),
    ("Legend for SP_xx sheets", bold),
    ("X (grey) = reported in EUForDam with damage > 0 in at least one year 2000–2022.", body),
    ("0 (light grey) = series exists but all values are 0 – expert may confirm presence/absence.", body),
    ("Blank yellow = expert enters confidence that the species occurs there: 0 = absent, 1 = low (possible), 2 = medium (likely), 3 = high (confirmed).", body),
    ("'Reported nationally' column (countries without NUTS data) = species reported at national level; experts indicate in which NUTS it occurs.", body),
    ("", body),
    ("Caveats", bold),
    ("Target units = Eurostat NUTS 2024 codes and names, checked against the official GISCO Nuts2json files (all codes valid, lists complete). NUTS 3 used where NUTS 2 has ≤4 units (HR, LT, SI, SK, RS) and for FVG. EUForDam FI codes are NUTS 2024 codes (Finland recoded several NUTS 3 units in 2024).", body),
    ("Abiotic sheet: filled from the Incoming sheets for all 16 countries (see Incoming_Check). Column 'Previous draft v9 (unverified)' holds what v9 wrote.", body),
    ("CZ: Ips typographus is stored under the spruce bark-beetle group code (CambXyl_con&BB_con&SM); it is counted as Ips typographus in SP_CZ.", body),
]
for i, (t, f) in enumerate(lines, 1):
    c = ws.cell(i, 1, t)
    c.font, c.alignment = f, Alignment(wrap_text=True, vertical="top")
ws.column_dimensions["A"].width = 130

# ---------------------------------------------------------------- Inventory_Countries
ws = wb.create_sheet("Inventory_Countries")
order = SUBNAT + [c for c in ["BG", "HR", "HU", "IT", "LT", "RO", "RS", "SK", "SI", "SE", "CH"]]
rows = []
for i, c in enumerate(order):
    g = d[d.Country == c]
    units = sorted(g.NUTS.unique())
    s = ser[ser[0] == c]
    nz_codes = g.loc[g.v > 0, "sp_code"].unique()
    sp = sorted({AGENT[k][0] for k in nz_codes if AGENT[k][1] != "group code"})
    grp = sorted({AGENT[k][0] for k in nz_codes if AGENT[k][1] == "group code"})
    if c == "CZ":
        sp = ["Ips typographus (inside spruce group code)"] + sp
    yrs = g.loc[g.v.notna(), "d_year"]
    sub = c in SUBNAT
    r = 5 + i
    rows.append([c, COUNTRY[c], LEVEL.get(c, "National"), len(units), ", ".join(units) if sub else c,
                 f"{yrs.min()}–{yrs.max()}", int(yrs.max()), len(s), ", ".join(sp), "; ".join(grp) or "–",
                 NUTS_STATUS.get(c, "NO"), REQ_SUB if sub else REQ_NAT,
                 f"=Abiotic!H{r}", f"=Abiotic!M{r}",
                 f"{TARGET[c][0]}, {len(TARGET[c][1])} units" if c in TARGET else "as held",
                 FLAGS.get(c, "")])
ws["A1"] = "Inventory per country – what we hold and what to request"
ws["A1"].font = title
ws["A2"] = "Red = no NUTS-level data → full series 2000–2025. Green = NUTS data → 2023–2025 only. Amber = held sub-nationally but needs a decision."
ws["A2"].font = body
hdr = ["Code", "Country", "Spatial level held", "# units", "Units held", "Years held", "Last year",
       "# agent series", "Species reported (damage > 0)", "Group codes (no species)", "NUTS-level data?",
       "REQUEST – insects", "REQUEST – wind", "REQUEST – total abiotic", "Target units", "Gaps / to clarify"]
table(ws, 4, hdr, rows, [6, 12, 22, 7, 30, 10, 7, 8, 45, 30, 16, 40, 28, 28, 20, 55])
for i, c in enumerate(order):
    st = NUTS_STATUS.get(c, "NO")
    fill = red if st == "NO" else (green if st == "Yes" else amber)
    for j in (1, 2, 11, 12):
        ws.cell(5 + i, j).fill = fill
n_ctry = len(order)

# ---------------------------------------------------------------- Inventory_Series
ws = wb.create_sheet("Inventory_Series")
hdr = ["Code", "Country", "Agent code", "Agent name", "Type", "Host", "Unit", "# spatial units",
       "First year with values", "Last year with values", "Blank years (2000–2022)", "# unit-years > 0",
       "# units with only zeros", "REQUEST", "Optional / to clarify"]
ser_sorted = ser.assign(o=ser[0].map({c: i for i, c in enumerate(order)})).sort_values(["o", 4, 3]).drop(columns="o")
table(ws, 1, hdr, ser_sorted.values.tolist(), [6, 12, 22, 34, 12, 16, 6, 8, 9, 9, 16, 9, 9, 22, 30])
for i, row in enumerate(ser_sorted.values.tolist(), 2):
    ws.cell(i, 14).fill = green if row[0] in SUBNAT else red

# ---------------------------------------------------------------- Abiotic
ws = wb.create_sheet("Abiotic")
ws["A1"] = "Abiotic damage – fill the yellow cells from the Incoming sheets; REQUEST columns are formulas"
ws["A1"].font = title
ws["A2"] = ("In Incoming? = Y/N. Level = NUTS (any sub-national NUTS/district series) or National. "
            "Years = first and last year with values. Rule: not in Incoming or national → full series 2000–2025 at NUTS; NUTS → update to 2025.")
ws["A2"].font = body
ws["A2"].alignment = Alignment(wrap_text=True)
ws.row_dimensions[2].height = 30
hdr = ["Code", "Country", "Insect data at NUTS level?",
       "WIND in Incoming? (Y/N)", "Wind level (NUTS/National)", "Wind first year", "Wind last year", "REQUEST – wind",
       "TOTAL ABIOTIC in Incoming? (Y/N)", "Total abiotic level (NUTS/National)", "Total abiotic first year",
       "Total abiotic last year", "REQUEST – total abiotic", "Notes (from Incoming)", "Previous draft v9 (unverified)"]


def req(inc, lev, first, last):
    return (f'=IF({inc}="","← fill from Incoming",IF(OR({inc}="N",{lev}="National"),'
            f'"Full series 2000–2025 at NUTS level",IF({last}="","← enter last year",'
            f'IF({last}>=2025,"Complete – nothing to request","Update "&({last}+1)&"–2025"'
            f'&IF(AND({first}<>"",{first}>2000),"  (optional: 2000–"&({first}-1)&")","")))))')


old = old_abio.set_index("Code")
rows = []
for i, c in enumerate(order):
    r = 5 + i
    prev = ""
    if c in old.index:
        o = old.loc[c]
        prev = (f"Wind: {o['WIND in Incoming? (Y/N)']} / {o['Wind level (District/National)']} / {o['Wind years']}; "
                f"Total: {o['TOTAL ABIOTIC in Incoming? (Y/N)']} / {o['Total abiotic level (District/National)']} / "
                f"{o['Total abiotic years']}. {o['Notes']}").replace("nan", "–")
    a = ABIO.get(c, (None,) * 9)
    rows.append([c, COUNTRY[c], NUTS_STATUS.get(c, "NO"), a[0], a[1], a[2], a[3],
                 req(f"D{r}", f"E{r}", f"F{r}", f"G{r}"), a[4], a[5], a[6], a[7],
                 req(f"I{r}", f"J{r}", f"K{r}", f"L{r}"), a[8], prev])
table(ws, 4, hdr, rows, [6, 12, 14, 10, 12, 9, 9, 30, 10, 12, 9, 9, 30, 40, 60])
dv_yn = DataValidation(type="list", formula1='"Y,N"', allow_blank=True)
dv_lev = DataValidation(type="list", formula1='"NUTS,National"', allow_blank=True)
dv_yr = DataValidation(type="whole", operator="between", formula1="1990", formula2="2026", allow_blank=True)
for dv in (dv_yn, dv_lev, dv_yr):
    ws.add_data_validation(dv)
last = 4 + n_ctry
for col, dv in (("D", dv_yn), ("I", dv_yn), ("E", dv_lev), ("J", dv_lev), ("F", dv_yr), ("G", dv_yr),
                ("K", dv_yr), ("L", dv_yr)):
    dv.add(f"{col}5:{col}{last}")
for r in range(5, last + 1):
    for col in "DEFGIJKLN":
        ws[f"{col}{r}"].fill = yellow
    ws[f"O{r}"].font = Font(name=F, size=9, italic=True, color="808080")
for col in ("H", "M"):
    rng = f"{col}5:{col}{last}"
    ws.conditional_formatting.add(rng, FormulaRule(formula=[f'LEFT({col}5,4)="Full"'], fill=red))
    ws.conditional_formatting.add(rng, FormulaRule(formula=[f'LEFT({col}5,6)="Update"'], fill=green))

# ---------------------------------------------------------------- Incoming_Check
ws = wb.create_sheet("Incoming_Check")
rows = []
for c in order:
    if c in INCOMING:
        rows.append([c, COUNTRY[c], *INCOMING[c]])
    else:
        rows.append([c, COUNTRY[c], "not yet received", "", "", "", "", "", ""])
table(ws, 1, ["Code", "Country", "Incoming file", "Spatial level", "Years", "Newer than EUForDam (2023+)?",
              "Agents in Incoming", "Agents in Incoming but NOT in EUForDam", "Issues / to clarify"],
      rows, [6, 12, 34, 28, 16, 12, 45, 34, 70])
for i, row in enumerate(rows, 2):
    ws.cell(i, 3).fill = green if row[0] in INCOMING else lgrey
    if "ERROR" in row[8] or "fake zeros" in row[8]:
        ws.cell(i, 9).fill = amber
ws.cell(len(rows) + 3, 1, f"Checked: {len(INCOMING)} of {len(order)} countries.").font = bold

# ---------------------------------------------------------------- Zero_Only_Series
ws = wb.create_sheet("Zero_Only_Series")
zero = pd.DataFrame(zero_rows).sort_values([0, 3, 4])
table(ws, 1, ["Code", "Country", "Agent code", "Agent name", "Unit code", "Unit name"],
      zero.values.tolist(), [6, 12, 22, 34, 12, 26])
for i, row in enumerate(zero.values.tolist(), 2):
    if row[0] == "DE" and row[2] == "ips_typ" and row[4] == "DE9":
        for j in range(1, 7):
            ws.cell(i, j).fill = amber

# ---------------------------------------------------------------- Agent_Codes
ws = wb.create_sheet("Agent_Codes")
nzc = d[d.v > 0].groupby("sp_code").Country.agg(lambda s: ", ".join(sorted(s.unique())))
rows = [[k, v[0], v[1], host.loc[k, "HOSTTREE"], host.loc[k, "SP_CATEGORY"], unit_of.get(k, ""),
         nzc.get(k, ""), len(nzc.get(k, "").split(", ")) if nzc.get(k, "") else 0] for k, v in AGENT.items()]
rows.sort(key=lambda r: (-r[7], r[1]))
table(ws, 1, ["Agent code", "Name used here (verify)", "Type", "Host", "Category", "Unit",
              "Countries with damage > 0", "# countries"], rows, [24, 44, 12, 16, 14, 6, 50, 9])

# ---------------------------------------------------------------- Species_List
MIN_C = 3
sel = [r for r in rows if r[2] != "group code" and r[7] >= MIN_C]
cand = [r for r in rows if r[2] != "group code" and r[7] < MIN_C]
ws = wb.create_sheet("Species_List")
ws["A1"] = f"Species in the presence sheets: species-level agents with damage > 0 in at least {MIN_C} countries ({len(sel)} species)"
ws["A1"].font = title
table(ws, 3, ["Agent code", "Name", "Host", "# countries", "Countries", "In SP_xx sheets?"],
      [[r[0], r[1], r[3], r[7], r[6], "Yes"] for r in sel] +
      [[r[0], r[1], r[3], r[7], r[6], "No – candidate"] for r in cand], [22, 40, 16, 9, 50, 16])
for i in range(len(sel)):
    ws.cell(4 + i, 6).fill = green
note_r = 5 + len(sel) + len(cand)
ws.cell(note_r, 1, "To add a candidate, move it into the list and rebuild. Agents not in EUForDam but often named in Incoming "
        "(e.g. Corythucha arcuata) could also be added.").font = body
SPECIES = [r[0] for r in sel]

# ---------------------------------------------------------------- Target_NUTS
ws = wb.create_sheet("Target_NUTS")
rows, UNITS = [], {}
for c in order:
    if c in SUBNAT:
        us = sorted(d[d.Country == c].NUTS.unique())
        if c == "FI":
            us = us + ["FI1D7", "FI200"]
        UNITS[c] = [(u, UNIT_NAMES.get(u, u)) for u in us]
        lvl = LEVEL[c]
        src = "units in EUForDam_REW" + ("" if c == "PL" else "; codes checked vs Eurostat NUTS 2024")
    else:
        lvl, UNITS[c] = TARGET[c]
        src = "Eurostat NUTS 2024 (checked)"
    for u, nm in UNITS[c]:
        note = "not in data – ask" if u in ("FI1D7", "FI200") else ""
        rows.append([c, COUNTRY[c], lvl, u, nm, src, note])
table(ws, 1, ["Code", "Country", "Level", "Unit code", "Unit name", "Source", "Note"], rows, [6, 12, 34, 12, 32, 24, 18])

# ---------------------------------------------------------------- SP_xx
dv_conf_text = '"0,1,2,3"'
for c in order:
    ws = wb.create_sheet(f"SP_{c}")
    g = d[d.Country == c].copy()
    if c == "SK":  # ips_dup in EUForDam = 'Other bark beetles' (mislabelled) -> do not mark
        g = g[g.sp_code != "ips_dup"]
    if c == "CZ":  # Ips typographus stored under spruce group code
        g.loc[g.sp_code == "CambXyl_con&BB_con&SM", "sp_code"] = "ips_typ"
    pos = g[g.v > 0].groupby("sp_code").NUTS.agg(set).to_dict()
    exist = g.groupby("sp_code").NUTS.agg(set).to_dict()
    sub = c in SUBNAT
    units = UNITS[c]
    ws["A1"] = f"{COUNTRY[c]} – species presence by {'unit' if sub else 'NUTS'} (EUForDam 2000–2022)"
    ws["A1"].font = title
    ws["A2"] = ("X = reported (damage > 0). 0 = series exists, all zeros. Yellow = please enter confidence that the species "
                "occurs there but is not recorded: 0 = absent, 1 = low, 2 = medium, 3 = high. Leave blank if unknown.")
    ws["A2"].font = body
    ws.merge_cells(start_row=2, start_column=1, end_row=2, end_column=min(4 + len(units), 20))
    ws["A2"].alignment = Alignment(wrap_text=True)
    ws.row_dimensions[2].height = 30
    fixed = ["Species", "Host", "Code"] + ([] if sub else ["Reported nationally"])
    nf = len(fixed)
    for j, h in enumerate(fixed, 1):
        ws.cell(4, j, h)
        ws.merge_cells(start_row=4, start_column=j, end_row=5, end_column=j)
    for k, (u, nm) in enumerate(units):
        ws.cell(4, nf + 1 + k, u)
        ws.cell(5, nf + 1 + k, nm)
    ccol = nf + len(units) + 1
    ws.cell(4, ccol, "Comments")
    ws.merge_cells(start_row=4, start_column=ccol, end_row=5, end_column=ccol)
    for r in (4, 5):
        for j in range(1, ccol + 1):
            cell = ws.cell(r, j)
            cell.font, cell.fill, cell.alignment, cell.border = hdr_font, hdr_fill, center, box
    ws.row_dimensions[5].height = 45
    dv = DataValidation(type="list", formula1=dv_conf_text, allow_blank=True)
    ws.add_data_validation(dv)
    r = 6
    for code in SPECIES:
        name = AGENT[code][0]
        ws.cell(r, 1, name).font = Font(name=F, size=10, italic=True)
        ws.cell(r, 2, host.loc[code, "HOSTTREE"]).font = body
        ws.cell(r, 3, code).font = Font(name=F, size=8, color="808080")
        if not sub:
            cell = ws.cell(r, 4)
            if code in pos:
                cell.value, cell.fill = "X", grey
            elif code in exist:
                cell.value, cell.fill = "0", lgrey
            cell.alignment, cell.font = center, bold
        for k, (u, _) in enumerate(units):
            cell = ws.cell(r, nf + 1 + k)
            cell.alignment, cell.font = center, bold
            if sub and u in pos.get(code, set()):
                cell.value, cell.fill = "X", grey
            elif sub and u in exist.get(code, set()):
                cell.value, cell.fill = "0", lgrey
            else:
                cell.fill = yellow
                dv.add(cell.coordinate)
        for j in range(1, ccol + 1):
            ws.cell(r, j).border = box
        r += 1
    r += 1
    ws.cell(r, 1, "Other species present but not in the list (name / units / confidence):").font = bold
    for _ in range(4):
        r += 1
        for j in range(1, ccol + 1):
            ws.cell(r, j).border = box
            ws.cell(r, j).fill = yellow
    ws.column_dimensions["A"].width = 34
    ws.column_dimensions["B"].width = 15
    ws.column_dimensions["C"].width = 9
    if not sub:
        ws.column_dimensions["D"].width = 11
    for k in range(len(units)):
        ws.column_dimensions[get_column_letter(nf + 1 + k)].width = 11
    ws.column_dimensions[get_column_letter(ccol)].width = 35
    ws.freeze_panes = ws.cell(6, nf + 1)

for s in wb.worksheets:
    s.sheet_view.zoomScale = 90
wb.save(OUT)
print("saved", OUT, "species:", len(SPECIES), [AGENT[s][0] for s in SPECIES])
print("zero-only rows:", len(zero_rows), "series:", len(ser))
