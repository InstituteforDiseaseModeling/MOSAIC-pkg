#' Get WHO Vaccination Data
#'
#' This function processes raw WHO vaccination request data and converts it into a format suitable for use in the MOSAIC cholera model.
#' It extracts relevant information from tab-separated text data for various years, such as the year, country, request number, status,
#' context, ICG decision date, number of doses requested, approved, and shipped, as well as the date of the vaccination campaign.
#' The data is compiled into a data frame and saved to a CSV format for further analysis.
#'
#' @param PATHS A list of file paths, which should include the following element:
#'   \describe{
#'     \item{DATA_SCRAPE_WHO_VACCINATION}{The path to the folder where the processed vaccination data will be saved as a CSV file.}
#'   }
#'
#' @source The data used by this function is sourced from the WHO ICG (International Coordinating Group on Vaccine Provision) dashboard,
#' which can be accessed via the following Power BI link: \url{https://app.powerbi.com/view?r=eyJrIjoiYmFmZTBmM2EtYWM3Mi00NWYwLTg3YjgtN2Q0MjM5ZmE1ZjFkIiwidCI6ImY2MTBjMGI3LWJkMjQtNGIzOS04MTBiLTNkYzI4MGFmYjU5MCIsImMiOjh9}
#'
#' @return A data frame containing the processed WHO vaccination request data. The function also prints summary information,
#' including the total number of observations, total requested doses, approved doses, shipped doses, and the start and end dates of the requests.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Reads raw vaccination request data from tab-separated text for multiple years (e.g., 2016-2024).
#'   \item Extracts relevant columns such as Year, Country, Request Number, Status, Context, Decision Date, Doses Requested, Approved, and Shipped.
#'   \item Handles missing values and checks for duplicated rows, removing duplicates and printing which rows were removed.
#'   \item Summarizes the data, printing the total number of observations, total doses requested, approved, and shipped, as well as the first and last decision dates.
#'   \item Saves the processed vaccination data to a CSV file in the location specified by the \code{PATHS} argument.
#' }
#'
#' @examples
#' \dontrun{
#' PATHS <- list(DATA_SCRAPE_WHO_VACCINATION = "path/to/save")
#' who_data <- get_WHO_vaccination_data(PATHS)
#' }
#'
#' @export

get_WHO_vaccination_data <- function(PATHS) {

     message("Getting raw text extraction from WHO OCV dashboard")

     raw_text_2016 <- "
Year	Country	Request Number	Status	Context	ICG decision date	Number of Doses requested	Number of Doses approved	Total number of doses shipped (Round 1+ Round 2)	Date of Vaccination campaign implementation(1st round)
2016	DRC	#9,2016	Approved	Outbreak response	9/2/2016	680,740	680,740	680,750	9/30/2016
2016	DRC	#11,2016	Approved	Outbreak response, complement for round 2	10/10/2016	70,000	70,000	70,000
2016	Malawi	#2,2016	Approved	Outbreak response	1/25/2016	160,000	160,000	160,020	2/16/2016
2016	Malawi	#3,2016	Approved	Outbreak response	2/24/2016	40,000	40,000	40,005
2016	Malawi	#10,2016	Not approved	Hotspot context	9/28/2016	180,000	0	0
2016	Mozambique	#6,2016	Approved	Outbreak response	8/11/2016	425,486	425,486	425,490	10/3/2016
2016	Niger	#1,2016	Approved	Humanitarian crisis	1/14/2016	195,132	195,132	195,160	2/13/2016
2016	South Sudan	#5,2016	Approved	Humanitarian crisis	4/1/2016	72,430	72,430	72,450	5/10/2016
2016	South Sudan	#7,2016	Cancelled by requestor	Outbreak response	8/18/2016	172,835	0	0
2016	Zambia	#4,2016	Approved	Outbreak response	3/28/2016	598,131	598,131	598,150	4/9/2016
2016	Zambia	#8,2016	Approved	Outbreak response (2nd dose)	8/15/2016	293,703	293,703	293,720	10/5/2016
"

     raw_text_2017 <- "
Year	Country	Request Number	Status	Context	ICG decision date	Number of Doses requested	Number of Doses approved	Total number of doses shipped (Round 1+ Round 2)	Date of Vaccination campaign implementation(1st round)
2017	Bangladesh	#12,2017	Approved	Humanitarian crisis	9/28/2017	900,000	900,000	900,025
2017	Chad	#13,2017	Approved	Outbreak response	10/25/2017	493,624	493,624	0
2017	Malawi	#5,2017	Approved	Outbreak response	4/7/2017	240,000	240,000	120,000	6/26/2017
2017	Mozambique	#4,2017	Approved	Outbreak response				329,630
2017	Mozambique	#4,2017	Approved	Outbreak response	4/5/2017	709,077	709,077	354,550	5/8/2017
2017	Nigeria	#11,2017	Approved	Outbreak response	9/6/2017	915,005	915,005	915,005	9/18/2017
2017	Nigeria	#14,2017	Approved	Outbreak response	10/30/2017	896,919	896,919	896,919
2017	Nigeria	#15,2017	Not approved	Preventive campaign	10/30/2017	580,320	0	0
2017	Sierra Leone	#9,2017	Approved	Humanitarian crisis	8/31/2017	1,036,209	1,036,209	1,036,300	9/15/2017
2017	Somalia	#3,2017	Approved	Outbreak response		264,600	264,600	718,200
2017	Somalia	#3,2017	Approved	Outbreak response	4/5/2017	988,042	907,202	453,600	5/2/2017
2017	Somalia	#7,2017	Re-directed to GTFCC**	Defined as preventive campaign		96,000		0
2017	Somalia	#8,2017	Re-directed to GTFCC**	Defined as preventive campaign		142,099		0
2017	South Sudan	#1,2017	Partially approved	Outbreak response	2/18/2017	304,098	68,967	69,125	2/27/2017
2017	South Sudan	#2,2017	Approved	Outbreak response	3/7/2017	474,976	474,976	474,985	3/15/2017
2017	The Philippines	#10,2017	Not approved	Outbreak response	9/1/2017	8,000		0
2017	Yemen	#6,2017	Partially approved	Outbreak response	6/15/2017	3,500,000	500,000	500,000
"

     raw_text_2018 <- "
Year	Country	Request Number	Status	Context	ICG decision date	Number of Doses requested	Number of Doses approved	Total number of doses shipped (Round 1+ Round 2)	Date of Vaccination campaign implementation(1st round)
2018	Bangladesh	#3,2018	Approved	Humanitarian crisis	4/11/2018	849,906	984,906	984,900	5/6/2018
2018	Bangladesh	#13,2018	Approved	Humanitarian crisis	10/19/2018	328,556	328,556	328,580	11/3/2018
2018	DRC	#1,2018	Approved	Outbreak response	1/25/2018	2,381,416	2,381,416	0
2018	DRC	#6,2018	Approved	Outbreak response	5/15/2018	69,846	158,917	0
2018	Lao PDR	#9,2018	Approved	Humanitarian crisis	8/20/2018	19,638	19,638	19,700	8/25/2018
2018	Malawi	#2,2018	Approved	Outbreak response	3/2/2018	500,600	500,600	500,600
2018	Malawi	#2,2018	Approved	Outbreak response	3/2/2018	500,600	500,600	500,600	4/17/2018
2018	Niger	#10,2018	Not approved	Outbreak response	9/3/2018	1,099,652		0
2018	Niger	#12,2018	Partially approved	Outbreak response	10/3/2018	2,293,776	317,330	317,380	12/3/2018
2018	Nigeria	#4,2018	Approved	Outbreak response	4/12/2018	1,315,152	1,182,988	1,183,000	5/9/2018
2018	Nigeria	#5,2018	Approved	Outbreak response	5/4/2018	291,000	252,872	252,900	5/9/2018
2018	Nigeria	#7,2018	Approved	Outbreak response	6/13/2018	757,630	757,630	757,800	7/10/2018
2018	South Sudan	#8,2018	Approved	Humanitarian crisis	6/18/2018	105,886	96,250	96,250	6/13/2018
2018	Zimbabwe	#11,2018	Approved	Outbreak response	9/27/2018	2,763,358	2,763,358	2,763,400	10/3/2018
"

raw_text_2019 <- "
Year	Country	Request Number	Status	Context	ICG decision date	Number of Doses requested	Number of Doses approved	Total number of doses shipped (Round 1+ Round 2)	Date of Vaccination campaign implementation(1st round)
2019	Bangladesh	#17,2019	Approved	Outbreak response	11/12/2019	1,289,223	1,270,170	635,085	12/8/2019
2019	Bangladesh	#17,2020	Approved 2nd Round (Additional doses requested for the 2nd round and approved on 17 January 2020)	Outbreak response			149,782	0
2019	Burundi	#16,2019	Not approved	Outbreak response	11/15/2019	4,933,182	0	0
2019	Cameroon	#3,2019	Approved	Outbreak response	4/15/2019	1,348,278	1,297,538	680,960	8/1/2019
2019	Cameroon	#3,2019	Approved (2nd round)	Outbreak response				616,595
2019	Cameroon	#3,2019	Approved (change in strategy)	Outbreak response		1,126,000	1,126,000	691,200
2019	Cameroon	#19,2019	Approved	Outbreak response	12/9/2019	67,673	67,672	67,690	12/20/2019
2019	DRC	#4,2019	Not approved	Outbreak response	4/15/2019	1,776,890	0	0
2019	DRC	#5,2019	Partially approved	Outbreak response	4/15/2019	2,119,078	1,670,366	835,190	5/28/2019
2019	DRC	#5,2019	Partially approved (2nd Round)	Outbreak response				835,190	10/30/2019
2019	DRC	#14,2019	Approved	Outbreak response	10/28/2019	961,656	961,656	961,670	1/15/2020
2019	Ethiopia	#7,2019	Not approved	Preventive campaign	4/16/2019	1,324,444	0	0
2019	Ethiopia	#9,2019	Approved	Preventive campaign	5/10/2019	817,705	778,766	778,700	8/15/2019
2019	Iran	#10,2019	Not approved	Preventive campaign	5/21/2019	280,744	0	0
2019	Mozambique	#1,2019	Approved	Humanitarian crisis	3/22/2019	1,769,906	1,769,906	884,975	4/3/2019
2019	Mozambique	#1,2019	Approved (2nd round)	Humanitarian crisis	6/14/2019			849,485	7/7/2019
2019	Mozambique	#8,2019	Partially approved	Outbreak response	5/2/2019	1,854,444	518,998	519,000	5/16/2019
2019	Sierra Leone	#11,2019	Cancelled by the requestor	Humanitarian crisis				0
2019	South Sudan	#18,2019	Approved	Humanitarian crisis	12/3/2019	508,784	508,784	254,400	4/2/2020
2019	Sudan	#12,2019	Approved	Outbreak response	9/23/2019	3,297,320	3,297,320	1,648,700	10/11/2019
2019	Sudan	#12,2019	Approved (2nd round)	Outbreak response				1,648,700	11/17/2019
2019	Sudan	#15,2019	Not approved	Preventive campaign	11/1/2019	5,663,594	0	0
2019	Zambia	#6,2019	Approved	Outbreak response	4/23/2019	136,834	136,834	136,840	5/22/2019
2019	Zambia	#6,2019	Approved (change in strategy)	Outbreak response		131,840	111,244	111,250	7/17/2019
2019	Zambia	#13,2019	Not approved	Preventive campaign	10/23/2019	136,834	0	0
2019	Zimbabwe	#2,2019	Approved	Humanitarian crisis	4/3/2019	975,646	975,646	975,650	4/16/2019
"

raw_text_2020 <- "
Year	Country	Request Number	Status	Context	ICG decision date	Number of Doses requested	Number of Doses approved	Total number of doses shipped (Round 1+ Round 2)	Date of Vaccination campaign implementation(1st round)
2020	DRC	#1,2020	Not approved	Outbreak response	2/10/2020	2,062,276	0	0
2020	DRC	#3,2020	Approved	Outbreak response	5/22/2020	2,103,576	2,103,576	2,103,600	7/28/2020
2020	Ethiopia	#5,2020	Partially approved	Outbreak / humanitarian	11/19/2020	4,938,802	3,354,278	3,354,400	12/28/2020
2020	Mozambique	#3,2020	Partially approved	Outbreak response	3/12/2020	976,816	733,424	733,500	9/21/2020
2020	South Sudan	#6,2020	Not approved	Outbreak response	12/30/2020	610,810	0	0
2020	Uganda	#4,2020	Approved	Outbreak response	6/2/2020	157,698	157,698	157,700	6/15/2020
"

raw_text_2021 <- "
Year	Country	Request Number	Status	Context	ICG decision date	Number of Doses requested	Number of Doses approved	Total number of doses shipped (Round 1+ Round 2)	Date of Vaccination campaign implementation(1st round)
2021	Bangladesh	#4,2021	Approved	Outbreak response	8/30/2021	2,836,820	2,711,896	1,441,926	10/10/2021
2021	Cameroon	#9,2021	Approved	Outbreak response	12/28/2021	409,532	409,532	204,800	2/18/2022
2021	Ethiopia	#1,2021	Partially approved	Outbreak response	3/5/2021	5,501,446	2,390,454	2,390,600	4/22/2021
2021	Ethiopia	#2,2021	Approved	Humanitarian	5/13/2021	4,017,218	4,017,218	2,008,650	6/10/2021
2021	Ethiopia	#8,2021	Approved	Outbreak response	11/4/2021	2,077,959	2,077,959	2,077,960	12/23/2021
2021	Nepal	#7,2021	Partially approved	Outbreak response	11/3/2021	1,106,328	504,726	483,408	11/21/2021
2021	Nepal	#10,2021	Approved	Outbreak response	12/21/2021	703,078	703,078	703,104	5/31/2022
2021	Niger	#5,2021	Approved	Outbreak response	9/10/2021	3,972,342	3,972,342	3,972,400	11/22/2021
2021	Nigeria	#3,2021	Approved	Outbreak response	7/8/2021	1,565,558	1,565,558	1,565,600	7/24/2021
2021	Nigeria	#6,2021	Approved	Outbreak response	9/14/2021	3,566,628	3,566,628	3,566,640	10/20/2021
"

raw_text_2022 <- "
Year	Country	Request Number	Status	Context	ICG decision date	Number of Doses requested	Number of Doses approved	Total number of doses shipped (Round 1+ Round 2)	Date of Vaccination campaign implementation(1st round)
2022	Bangladesh	#3,2022	Approved	Outbreak response	4/11/2022	4,749,952	4,749,952	4,749,956	6/26/2022
2022	Cameroon	#1,2022	Approved	Outbreak response	2/17/2022	1,684,171	1,684,171	1,684,200	4/8/2022
2022	Cameroon	#10,2022	Approved	Outbreak response	7/1/2022	4,285,406	4,285,406	2,142,703	8/31/2022
2022	Cameroon	#17,2022	Partially approved	Outbreak response	11/15/2022	4,401,742	1,565,113	1,565,113	2/22/2023
2022	DRC	#23,2022	Approved	Outbreak response	12/22/2022	728,274	364,137	364,137	1/25/2023
2022	Ethiopia	#20,2022	Partially approved	Outbreak response	12/2/2022	2,000,466	86,910	86,910	1/13/2023
2022	Haiti	#19,2022	Partially approved	Outbreak response	11/25/2022	1,789,744	2,166,712	2,166,712	12/19/2022
2022	Kenya	#8,2022	Not approved	Outbreak response	6/22/2022	7,699,134	0	0
2022	Kenya	#12,2022	Partially approved	Humanitarian crisis	9/12/2022	2,734,482	1,767,268	0
2022	Kenya	#21,2022	Partially approved	Outbreak response	12/9/2022	7,575,922	2,213,943	2,213,967	2/11/2023
2022	Lebanon	#14,2022	Approved	Outbreak response	10/25/2022	1,200,000	600,000	600,000	11/17/2022
2022	Lebanon	#18,2022	Partially approved	Outbreak response	11/24/2022	1,963,864	1,803,600	901,800	12/17/2022
2022	Malawi	#2,2022	Approved	Outbreak response	3/17/2022	3,895,390	3,895,390	1,947,700	5/23/2022
2022	Malawi	#13,2022	Approved	Outbreak response	10/20/2022	2,893,998	2,941,982	2,941,982	11/28/2022
2022	Pakistan	#5,2022	Approved	Outbreak response	5/26/2022	1,509,568	1,509,568	754,784	7/8/2022
2022	Pakistan	#7,2022	Approved	Outbreak response	6/7/2022	1,181,757	1,181,757	590,878	8/30/2022
2022	Pakistan	#9,2022	Approved	Outbreak response	6/27/2022	2,401,833	2,401,833	1,200,916	8/10/2022
2022	Pakistan	#11,2022	Approved	Outbreak response	8/24/2022	866,390	866,390	433,195	10/10/2022
2022	Somalia	#4,2022	Approved	Outbreak response	4/26/2022	1,869,022	1,869,022	1,869,024	6/14/2022
2022	Somalia	#15,2022	Approved	Outbreak response	11/10/2022	1,991,771	995,886	995,904	1/22/2023
2022	South Sudan	#6,2022	Not approved	Outbreak response	6/1/2022	1,027,580	0	0
2022	Syria	#16,2022	Approved	Outbreak response	11/10/2022	2,000,000	2,000,000	2,000,000	12/18/2022
2022	Syria	#22,2022	Partially approved	Outbreak response	12/29/2022	1,724,383	1,702,383	1,702,383	3/7/2023
"

raw_text_2023 <- "
Year	Country	Request Number	Status	Context	ICG decision date	Number of Doses requested	Number of Doses approved	Total number of doses shipped (Round 1+ Round 2)	Date of Vaccination campaign implementation(1st round)
2023	Cameroon	#11,2023	Partially approved	Outbreak response	6/8/2023	4,195,020	1,825,075	1,825,075	8/16/2023
2023	Dominican Republic (the)	#1,2023	Approved	Outbreak response	1/8/2023	85,000	85,000	85,000	1/25/2023
2023	DRC	#18,2023	Approved	Outbreak response	10/2/2023	5,011,828	5,011,828	5,011,828	12/11/2023
2023	Ethiopia	#8,2023	Approved	Outbreak response	3/29/2023	1,275,898	1,910,416	1,910,416	5/15/2023
2023	Ethiopia	#13,2023	Partially approved	Outbreak response	7/9/2023	4,489,549	2,230,038	2,230,038	8/3/2023
2023	Ethiopia	#15,2023	Approved	Outbreak response	8/3/2023	1,917,914	1,917,914	1,917,914	9/16/2023
2023	Ethiopia	#17,2023	Partially approved	Outbreak response	10/17/2023	1,962,168	862,352	862,352	11/27/2023
2023	Ethiopia	#19,2023	Approved	Outbreak response	10/4/2023	1,522,495	1,522,495	1,522,495	11/6/2023
2023	Ethiopia	#23,2023	Partially approved	Outbreak response	1/17/2024	1,793,948	542,764	288,000
2023	Ethiopia	#24,2023	Partially approved	Outbreak response	12/4/2023	2,075,719	1,223,254	1,223,254	1/11/2024
2023	Kenya	#10,2023	Partially approved	Outbreak response	6/8/2023	9,919,647	1,533,199	1,533,199	8/3/2023
2023	Kenya	#14,2023	Partially approved	Outbreak response	7/6/2023	1,913,064	175,575	175,584	8/3/2023
2023	Malawi	#3,2023	Partially approved	Outbreak response	3/30/2023	6,311,475	1,415,497	1,415,497	4/25/2023
2023	Mozambique	#2,2023	Approved	Outbreak response	2/1/2023	719,240	719,240	719,240	2/27/2023
2023	Mozambique	#4,2023	Approved	Outbreak response	3/9/2023	1,571,099	1,358,682	1,358,682	3/31/2023
2023	Mozambique	#7,2023	Approved	Outbreak response	3/21/2023	410,629	410,629	410,629	3/30/2023
2023	Mozambique	#16,2023	Partially approved	Outbreak response	9/15/2023	924,456	513,827	513,827	10/24/2023
2023	Mozambique	#22,2023	Approved	Outbreak response	12/4/2023	2,271,136	2,271,136	2,271,136	1/8/2024
2023	Somalia	#12,2023	Approved	Outbreak response	6/15/2023	590,803	590,803	590,810	8/12/2023
2023	South Sudan	#6,2023	Cancelled by the requestor	Outbreak response		262,136	0	0
2023	Sudan (the)	#20,2023	Partially approved	Outbreak response	10/25/2023	2,521,785	2,228,083	2,228,085	11/20/2023
2023	Sudan (the)	#21,2023	Approved	Outbreak response	10/31/2023	692,710	692,710	692,710	11/20/2023
2023	Sudan (the)	#25,2023	Partially approved	Outbreak response	1/5/2024	5,529,774	1,661,038	1,661,038	2/7/2024
2023	Syria	#5,2023	Approved	Outbreak response	3/23/2023	1,119,799	1,119,799	1,119,799	6/10/2023
2023	Zambia	#9,2023	Partially approved	Outbreak response	4/18/2023	5,106,193	628,226	628,320	6/5/2023
2023	Zambia	#26,2023	Partially approved	Outbreak response	12/28/2023	3,498,586	1,701,112	1,701,112	1/16/2024
2023	Zimbabwe	#27,2023	Partially approved	Outbreak response	1/11/2024	2,414,306	2,303,248	2,303,248	1/29/2023
"
raw_text_2024 <- "
Year	Country	Request Number	Status	Context	ICG decision date	Number of Doses requested	Number of Doses approved	Total number of doses shipped (Round 1+ Round 2)	Date of Vaccination campaign implementation(1st round)
2024	Bangladesh	#14,2024	Approved	Outbreak response	9/9/2024	1,635,691	1,635,691	1,635,700
2024	Comoros	#4,2024	Approved	Outbreak response	4/29/2024	872,301	872,301	872,301	6/19/2024
2024	Ethiopia	#1,2024	Partially approved	Outbreak response	1/17/2024	2,620,275	369,869	0
2024	Ethiopia	#7,2024	Partially approved	Outbreak response	6/25/2024	2,550,989	1,670,514	1,670,550
2024	Ethiopia	#17,2024	Approved	Outbreak response	10/3/2024	1,977,662	1,977,662	1,265,100
2024	Kenya	#5,2024	Partially approved	Outbreak response/pre-emptive	5/27/2024	2,702,568	135,907	0
2024	Mozambique	#8,2024	Approved	Outbreak response	6/19/2024	107,478	107,478	107,500	7/26/2024
2024	Myanmar	#12,2024	Partially approved	Outbreak response	8/20/2024	2,734,317	2,451,847	2,451,850
2024	Myanmar	#18,2024	Approved	Outbreak response	10/4/2024	70,243	70,243	0
2024	Niger	#16,2024	Approved	Outbreak response	9/27/2024	1,069,584	1,069,584	1,069,600
2024	Nigeria	#10,2024	Partially approved	Outbreak response	7/10/2024	6,129,353	4,472,355	4,472,400
2024	Somalia	#2,2024	Approved	Outbreak response	2/1/2024	1,862,799	1,399,391	1,399,391	4/28/2024
2024	Sudan (the)	#11,2024	Not approved	Pre-emptive campaign	7/17/2024	1,765,413	0	0
2024	Sudan (the)	#13,2024	Approved	Outbreak response	8/22/2024	455,081	455,081	404,100
2024	Sudan (the)	#15,2024	Approved	Outbreak response	9/11/2024	1,407,188	1,407,188	1,407,200
2024	Yemen	#6,2024	Partially approved	Outbreak response	7/29/2024	5,230,640	3,837,769	3,837,800
2024	Zambia	#3,2024	Partially approved	Outbreak response	2/7/2024	3,613,580	2,246,140	2,246,140	6/25/2024
2024	Zimbabwe	#9,2024	Partially approved	Outbreak response	6/24/2024	2,067,430	213,586	213,600
2024	Sudan (the)	#19,2024	Approved	Outbreak response	10/7/2024	2,237,252	2,237,252
"

message("Processing raw text")

     # Extract lines, but split by tabs instead of spaces
     get_lines <- function(raw_text) {
          # Split the raw text into lines
          lines <- strsplit(raw_text, "\n")[[1]]

          # Remove empty lines
          lines <- lines[lines != ""]

          # Confirm the first line contains "Year" and "Country" to ensure it is the header
          if (!(grepl("Year", lines[1]) & grepl("Country", lines[1]))) {
               stop("The first line is not the header. Ensure it contains 'Year' and 'Country'.")
          }

          return(lines)
     }

     # Extract column names and convert them to snake_case
     get_column_names <- function(raw_text) {
          lines <- get_lines(raw_text)
          header_line <- lines[1]  # Assume the first line is the header
          column_names <- unlist(strsplit(header_line, "\t"))  # Split by tabs
          column_names_snake <- tolower(gsub("[^a-zA-Z0-9]+$", "", gsub("[^a-zA-Z0-9]", "_", column_names)))
          return(column_names_snake)
     }

     # Extract the year
     get_year <- function(raw_text) {
          lines <- get_lines(raw_text)
          data_lines <- lines[-1]  # Remove the header
          years <- sapply(data_lines, function(line) unlist(strsplit(line, "\t"))[1])
          return(as.integer(years))
     }

     # Extract the country
     get_country <- function(raw_text) {
          lines <- get_lines(raw_text)
          data_lines <- lines[-1]  # Remove the header
          countries <- sapply(data_lines, function(line) unlist(strsplit(line, "\t"))[2])
          return(countries)
     }

     get_request_number <- function(raw_text) {

          lines <- get_lines(raw_text)
          data_lines <- lines[-1]  # Remove the header

          request_numbers <- rep(NA, length(data_lines))  # Initialize an integer vector

          # Loop through each data line
          for (i in seq_along(data_lines)) {

               fields <- unlist(strsplit(data_lines[i], "\t"))

               # Extract the year (first element) and the request number (third element)
               tmp <- unlist(strsplit(fields[3],"[#,]"))
               request_number_clean <- paste0(tmp[3], tmp[2])

               # Concatenate the year and request number
               request_numbers[i] <- as.integer(request_number_clean)
          }

          return(request_numbers)
     }

     # Extract status
     get_status <- function(raw_text) {
          lines <- get_lines(raw_text)
          data_lines <- lines[-1]  # Remove the header
          status <- sapply(data_lines, function(line) unlist(strsplit(line, "\t"))[4])
          return(status)
     }

     # Extract context
     get_context <- function(raw_text) {
          lines <- get_lines(raw_text)
          data_lines <- lines[-1]  # Remove the header
          contexts <- sapply(data_lines, function(line) unlist(strsplit(line, "\t"))[5])
          return(contexts)
     }

     # Extract decision date and convert to Date object
     get_decision_date <- function(raw_text) {
          lines <- get_lines(raw_text)
          data_lines <- lines[-1]  # Remove the header
          decision_dates <- sapply(data_lines, function(line) unlist(strsplit(line, "\t"))[6])
          decision_dates <- as.Date(decision_dates, format = "%m/%d/%Y")
          return(decision_dates)
     }

     # Extract number of doses requested
     get_doses_requested <- function(raw_text) {
          lines <- get_lines(raw_text)
          data_lines <- lines[-1]  # Remove the header
          doses_requested <- sapply(data_lines, function(line) gsub(",", "", unlist(strsplit(line, "\t"))[7]))
          return(as.integer(doses_requested))
     }

     # Extract number of doses approved
     get_doses_approved <- function(raw_text) {
          lines <- get_lines(raw_text)
          data_lines <- lines[-1]  # Remove the header
          doses_approved <- sapply(data_lines, function(line) gsub(",", "", unlist(strsplit(line, "\t"))[8]))
          return(as.integer(doses_approved))
     }

     # Extract number of doses shipped
     get_doses_shipped <- function(raw_text) {
          lines <- get_lines(raw_text)
          data_lines <- lines[-1]  # Remove the header
          doses_shipped <- sapply(data_lines, function(line) gsub(",", "", unlist(strsplit(line, "\t"))[9]))
          return(as.integer(doses_shipped))
     }

     get_campaign_date <- function(raw_text) {

          lines <- get_lines(raw_text)
          data_lines <- lines[-1]  # Remove the header
          campaign_dates <- rep(NA, length(data_lines))

          for (i in seq_along(data_lines)) {
               fields <- unlist(strsplit(data_lines[i], "\t"))
               campaign_dates[i] <- fields[length(fields)]
          }

          campaign_dates <- as.Date(campaign_dates, format = "%m/%d/%Y")
          return(campaign_dates)
     }

     # List of raw text data for different years
     raw_text_list <- list(raw_text_2016, raw_text_2017, raw_text_2018,
                           raw_text_2019, raw_text_2020, raw_text_2021,
                           raw_text_2022, raw_text_2023, raw_text_2024)

     # Function to process each raw text and convert to a data frame
     process_raw_text <- function(raw_text) {
          data.frame(
               year = get_year(raw_text),
               country = get_country(raw_text),
               request_number = get_request_number(raw_text),
               status = get_status(raw_text),
               context = get_context(raw_text),
               decision_date = get_decision_date(raw_text),
               doses_requested = get_doses_requested(raw_text),
               doses_approved = get_doses_approved(raw_text),
               doses_shipped = get_doses_shipped(raw_text),
               campaign_date = get_campaign_date(raw_text),
               stringsAsFactors = FALSE
          )
     }

     # Apply the function to each raw_text entry in the list and combine them into a single data frame
     who_data_list <- lapply(raw_text_list, process_raw_text)
     who_data <- do.call(rbind, who_data_list)
     row.names(who_data) <- NULL

     # Check for duplicated rows
     duplicate_rows <- duplicated(who_data)
     if (any(duplicate_rows)) {
          # Print the indices of duplicated rows
          duplicate_indices <- which(duplicate_rows)
          message("Duplicated rows found and removed: ", paste(duplicate_indices, collapse = ", "))

          # Remove duplicated rows
          who_data <- who_data[!duplicate_rows, ]
     }

     print(head(who_data))

     message("Start date: ", min(who_data$decision_date, na.rm = TRUE))
     message("End date: ", max(who_data$decision_date, na.rm = TRUE))
     message("Total doses requested: ", sum(who_data$doses_requested, na.rm = TRUE))
     message("Total number of observations: ", nrow(who_data))
     message("Total doses requested: ", sum(who_data$doses_requested, na.rm = TRUE))
     message("Total doses approved: ", sum(who_data$doses_approved, na.rm = TRUE))
     message("Total doses shipped: ", sum(who_data$doses_shipped, na.rm = TRUE))

     data_path <- file.path(PATHS$DATA_SCRAPE_WHO_VACCINATION, "who_vaccination_data.csv")
     write.csv(who_data, data_path, row.names = FALSE)
     message(paste("Raw vaccination data saved to:", data_path))

}

