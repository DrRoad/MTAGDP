##
##    Name:       organise_industries.R
##
##    Objective:  For some of the graphical output, it is desireable to place the industry levels
##                in some logical order (e.g. from Primary Industries to Goods-Producing to Service Industries).
##                This script creates functions to organise the different ANZSIC06 classes utilised in the
##                creation of MTAGDP.  Including:
##                  organise_leed4s()   # for the LEED table 4 industries
##                  organise_rgdps()    # for the Statistics New Zealand Regional GDP industries
##                  organise_ngdps()    # for the Statistics NZ National GDP industires
##
##   Approach:    Functons are designed to take a general column of a data set (here, defined
##                as 'class_name') for a given industry classification.  The default for each different
##                classification reflects the column name in the MTAGDP object, but could be used to
##                organise another column name (e.g. 'LEED4_industry'), depending on user needs.
##
##                Internal checks are made to ensure the levels match in the class_name column.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2015-05-29
##

##
## 1. LEED4Industries
##

  organise_leed4s <- function(x, class_name = "LEED4Industry") {
    x <- data.frame(x)  	
  	# set levels
  	  y <- factor(x[ , class_name],
  	                      levels = c("Nursery and floriculture production",
                                     "Mushroom and vegetable growing",
                                     "Fruit and tree nut growing",
                                     "Grain, sheep, and beef cattle farming",
                                     "Other crop growing",
                                     "Dairy cattle farming",
                                     "Poultry farming",
                                     "Deer farming",
                                     "Other livestock farming",
                                     "Forestry and logging",
                                     "Aquaculture",
                                     "Fishing",
                                     "Hunting and trapping",
                                     "Forestry support services",
                                     "Agriculture and fishing support services",
                                     "Coal, oil, gas, and metal ore mining",
                                     "Construction material mining",
                                     "Other non-metallic mineral mining and quarrying",
                                     "Exploration and other mining services",
                                     "Meat and meat product manufacturing",
                                     "Seafood processing",
                                     "Dairy product and other food product manufacturing",
                                     "Fruit and vegetable processing",
                                     "Grain mill and cereal product manufacturing",
                                     "Bakery product, sugar, and confectionery manufacturing",
                                     "Beverage and tobacco manufacturing",
                                     "Textile fibre, yarn, and woven fabric manufacturing",
                                     "Leather tanning and fur dressing",
                                     "Textile product manufacturing",
                                     "Knitted product manufacturing",
                                     "Clothing and footwear manufacturing",
                                     "Log sawmilling and timber dressing",
                                     "Other wood product manufacturing",
                                     "Pulp, paper, and converted paper product manufacturing",
                                     "Printing",
                                     "Petroleum refining and petroleum and coal product manufacturing",
                                     "Chemical manufacturing",
                                     "Basic polymer manufacturing",
                                     "Fertiliser and pesticide manufacturing",
                                     "Pharmaceutical and medicinal product manufacturing",
                                     "Other basic chemical product manufacturing",
                                     "Polymer product and rubber product manufacturing",
                                     "Glass and glass product manufacturing",
                                     "Ceramic product manufacturing",
                                     "Cement, lime, plaster and concrete product manufacturing",
                                     "Other non-metallic mineral product manufacturing",
                                     "Primary metal and metal product manufacturing",
                                     "Iron and steel forging",
                                     "Structural metal product manufacturing",
                                     "Metal container manufacturing",
                                     "Other sheet metal product manufacturing",
                                     "Other fabricated metal product manufacturing",
                                     "Motor vehicle and motor vehicle part manufacturing",
                                     "Other transport equipment manufacturing",
                                     "Professional and scientific equipment manufacturing",
                                     "Computer and electronic equipment manufacturing",
                                     "Electrical equipment and domestic appliance manufacturing",
                                     "Pump, compressor, heating, and ventilation equipment manufacturing",
                                     "Specialised machinery and equipment manufacturing",
                                     "Other machinery and equipment manufacturing",
                                     "Furniture manufacturing",
                                     "Other manufacturing",
                                     "Electricity and gas supply",
                                     "Water supply, sewerage, and drainage services",
                                     "Waste collection services",
                                     "Waste treatment, disposal, and remediation services",
                                     "Residential building construction",
                                     "Non-residential building construction",
                                     "Heavy and civil engineering construction",
                                     "Land development and site preparation services",
                                     "Building structure services",
                                     "Building installation services",
                                     "Building completion services",
                                     "Other construction services",
                                     "Agricultural product wholesaling",
                                     "Mineral, metal, and chemical wholesaling",
                                     "Timber and hardware goods wholesaling",
                                     "Specialised industrial machinery and equipment wholesaling",
                                     "Other machinery and equipment wholesaling",
                                     "Motor vehicle and motor vehicle parts wholesaling",
                                     "Grocery, liquor, and tobacco product wholesaling",
                                     "Textile, clothing, and footwear wholesaling",
                                     "Pharmaceutical and toiletry goods wholesaling",
                                     "Furniture, floor coverings, and other goods wholesaling",
                                     "Commission based wholesaling",
                                     "Motor vehicle retailing",
                                     "Motor vehicle parts retailing",
                                     "Fuel retailing",
                                     "Supermarket and grocery stores",
                                     "Specialised food retailing",
                                     "Furniture, floor coverings, houseware, and textile goods retailing",
                                     "Electrical and electronic goods retailing",
                                     "Hardware, building, and garden supplies retailing",
                                     "Recreational goods retailing",
                                     "Clothing, footwear, and personal accessories retailing",
                                     "Department stores",
                                     "Pharmaceutical and other store-based retailing",
                                     "Non store retailing",
                                     "Retail commission based buying and/or selling",
                                     "Accommodation",
                                     "Cafes, restaurants and takeaway food services",
                                     "Pubs, taverns and bars",
                                     "Clubs (hospitality)",
                                     "Road transport",
                                     "Rail, water, air, and space transport",
                                     "Scenic and sightseeing transport",
                                     "Pipeline and other transport",
                                     "Postal and other transport support services",
                                     "Water transport support services",
                                     "Air transport support services",
                                     "Warehousing and storage services",
                                     "Publishing (except music publishing)",
                                     "Motion picture and video activities",
                                     "Sound recording and music publishing",
                                     "Broadcasting (except internet)",
                                     "Telecommunications, internet, and data processing services",
                                     "Libraries and archives",
                                     "Central banking and non-depository financing",
                                     "Depository financial intermediation",
                                     "Financial asset investing",
                                     "Life insurance",
                                     "Health and general insurance",
                                     "Superannuation funds",
                                     "Auxiliary finance and investment services",
                                     "Auxiliary insurance services",
                                     "Motor vehicle and transport equipment rental and hiring",
                                     "Other goods and equipment rental and hiring",
                                     "Non-financial intangible assets (except copyrights) leasing",
                                     "Property operators",
                                     "Real estate services",
                                     "Scientific research services",
                                     "Architectural, engineering, and technical services",
                                     "Legal and accounting services",
                                     "Advertising services",
                                     "Market research and statistical services",
                                     "Management and other consulting services",
                                     "Veterinary services",
                                     "Other professional, scientific, and technical services",
                                     "Computer systems design and related services",
                                     "Travel agency services",
                                     "Employment services",
                                     "Other administrative services",
                                     "Building cleaning, pest control, and gardening services",
                                     "Packaging and labelling services",
                                     "Local government administration",
                                     "Central government administration",
                                     "Public order, safety, and regulatory services, and defence",
                                     "Preschool education",
                                     "School education",
                                     "Tertiary education",
                                     "Adult, community, and other education",
                                     "Hospitals",                                     
                                     "Medical services",
                                     "Pathology and diagnostic imaging services",
                                     "Allied health services",
                                     "Other health care services",
                                     "Residential care services",
                                     "Child care services",
                                     "Other social assistance services",                                     
                                     "Museum operation",
                                     "Parks and gardens operations",
                                     "Creative and performing arts activities",
                                     "Sport and physical recreation activities",
                                     "Horse and dog racing activities",
                                     "Amusement and other recreation activities",
                                     "Gambling activities",
                                     "Automotive repair and maintenance",
                                     "Machinery and equipment repair and maintenance",
                                     "Other repair and maintenance",
                                     "Personal care services",
                                     "Funeral, crematorium, and cemetery services",
                                     "Other personal services",
                                     "Religious services",
                                     "Civic, professional, and other interest group services",
                                     "Not elsewhere included",
                                     "GST on Production, Import Duties and Other Taxes"
                                    ))	
                  if(sum(is.na(y)) > 0){
                     stop("Some industries did not match in the sorting process or are NA")
                  }    
                                
                  return(y)
                                 
         }

##
## 2. Regional GDP Industries
##

   organise_rgdps <- function(x, class_name = "RGDP_industry") {
     x <- data.frame(x)
     
    # set levels
      y <- factor(x[ , class_name],
					      levels = c("Agriculture",
					                 "Forestry, Fishing, Mining, Electricity, Gas, Water and Waste Services",
					                 "Manufacturing",
					                 "Construction",
					                 "Wholesale Trade",
					                 "Retail Trade",
					                 "Accommodation and Food Services",
					                 "Transport, Postal and Warehousing",
					                 "Financial and Insurance Services",
					                 "Rental, Hiring and Real Estate Services",
					                 "Owner-Occupied Property Operation",
					                 "Professional, Scientific, Technical, Administrative and Support Services",
					                 "Public Administration and Safety",
					                 "Education and Training",
					                 "Health Care and Social Assistance",
					                 "Information Media, Telecommunications and Other Services",
					                 "GST on Production, Import Duties and Other Taxes"
					                 ))
					                 
                  if(sum(is.na(y)) > 0){
                     stop("Some industries did not match in the sorting process or are NA")
                  }                 
                   
                   return(y)
                                           
	       }		                     


##
## 3. National GDP Industries
##

organise_ngdps <- function(x, class_name = "NGDP_industry") {
    x <- data.frame(x)
    
   # set levels
     y <- factor(x[ , class_name],
				          levels = c("Horticulture and Fruit Growing",
						             "Sheep, Beef Cattle and Grain Farming",
						             "Dairy Cattle Farming",
						             "Poultry, Deer and Other Livestock Farming",
						             "Forestry and Logging",
						             "Fishing and Aquaculture",
						             "Agriculture, Forestry and Fishing Support Services and Hunting",
						             "Mining",
						             "Meat and Meat Product Manufacturing",
						             "Seafood Processing",
						             "Dairy Product Manufacturing",
						             "Fruit, Oil, Cereal and Other Food Product Manufacturing",
						             "Beverage and Tobacco Product Manufacturing",
						             "Textile, Leather, Clothing and Footwear Manufacturing",
						             "Wood Product Manufacturing",
						             "Pulp, Paper and Converted Paper Product Manufacturing",
						             "Printing",
						             "Petroleum and Coal Product Manufacturing",
						             "Basic Chemical and Chemical Product Manufacturing",
						             "Polymer Product and Rubber Product Manufacturing",
						             "Non-Metallic Mineral Product Manufacturing",
						             "Primary Metal and Metal Product Manufacturing",
						             "Fabricated Metal Product Manufacturing",
						             "Transport Equipment Manufacturing",
						             "Machinery and Other Equipment Manufacturing",
						             "Furniture and Other Manufacturing",
					                 "Electricity and Gas Supply",
						             "Water, Sewerage, Drainage and Waste Services",
						             "Building Construction",
						             "Heavy and Civil Engineering Construction",
						             "Construction Services",
						             "Wholesale Trade",
						             "Motor Vehicle and Motor Vehicle Parts and Fuel Retailing",
						             "Supermarket, Grocery Stores and Specialised Food Retailing",
						             "Other Store-Based Retailing and Non Store Retailing",
						             "Accommodation and Food Services",
						             "Road Transport",
						             "Rail, Water, Air and Other Transport",
						             "Postal, Courier Transport Support, and Warehousing Services.",
						             "Information Media Services",
						             "Telecommunications, Internet and Library Services",
						             "Finance",
						             "Insurance and Superannuation Funds",
						             "Auxiliary Finance and Insurance Services",
						             "Rental and Hiring Services (except Real Estate)",
						             "Property Operators and Real Estate Services",
						             "Owner-Occupied Property Operation (National Accounts Only)",
						             "Professional, Scientific and Technical Services",
						             "Administrative and Support Services",
						             "Local Government Administration",
						             "Central Government Administration, Defence and Public Safety",
						             "Education and Training",
						             "Health Care and Social Assistance",
						             "Arts and Recreation Services",
						             "Other Services",
						             "GST on Production, Import Duties and Other Taxes"
								    ))
								    
                  if(sum(is.na(y)) > 0){
                     stop("Some industries did not match in the sorting process or are NA")
                  }                  
                          
                            return(y)
                            
	     }
