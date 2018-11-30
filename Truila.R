library(httr)
library(bitops)
library(RCurl)
library(rvest)
library(XML)
library(xml2)
library(jsonlite)
library(curl)
library(civis)

#####   TRULIA   #####
#options(timeout= 4000000)
Tr.burl <- "https://www.trulia.com/for_rent/"
#Then the zip number
zips <- {c("02118",
           "02119",
           "02120",
           "02130",
           "02134",           
           "02135",
           "02445",
           "02446",
           "02447",
           "02467",
           "02108",
           "02114",
           "02115",
           "02116",
           "02215",
           "02128",
           "02129",
           "02150",
           "02151",
           "02152",
           "02124",
           "02126",
           "02131",
           "02132",
           "02136",
           "02109",
           "02110",
           "02111",
           "02113",
           "02121",
           "02122",
           "02124",
           "02125",
           "02127",
           "02210"
)}
#Zips nachwahl
znw <- "_zip/"
pnw <- "_p/"

curs <- read_civis("sandbox.trulia_master", database = "City of Boston")

mtr <- data.frame(Post_Title = as.character(),
                  Address = as.character(),
                  Price = as.character(),
                  Beds = as.character(),
                  Baths = as.character(),
                  SQFT = as.character(),
                  Pets = as.character(),
                  Desc = as.character(),
                  Scrape_Date = as.character(),
                  Scrape_Zip = as.character()
)


for (i in 1:length(zips)){
#for (i in 1){
  zurl <- paste(Tr.burl, zips[i], znw, sep = "")
  
  #####
  print("ZIP LANDING PAGE TRY CATCH STARTED")
  #####
  
  slp <- sample(1:6, 1)
  print(paste("Sleeping for", slp, "seconds at", Sys.time()))
  Sys.sleep(slp)
  
  chka <-  tryCatch({
    getURL(zurl) %>%
      read_html()
  },
  error = function(e){e}
  )
  
  if(inherits(chka, "error")) {
    print("URL Broken")
    next
  }
  
  #####
  print("ZIP LANDING PAGE TRY CATCH SUCCESS")
  #####
  
  slp <- sample(1:5, 1)
  print(paste("Sleeping for", slp, "seconds at", Sys.time()))
  Sys.sleep(slp)
  
  
  #Getting around their F**king 403 wall
  gh <- getURL(zurl) %>%
    read_html()
  
  #####
  print("ZIP LANDING PAGE READ")
  #####
  
  #Page Links
  pl <- gh %>%
    html_nodes(".xsCol12Landscape.smlCol12.lrgCol8") %>%
    html_nodes(".card.boxBasic.backgroundBasic") %>%
    html_children() %>%
    html_children() %>%
    html_attr("href") %>% 
    na.omit() %>%
    as.character() %>%
    unique()
  
  #Max Pages
  mp <- gh %>%
    html_nodes(".pvl.phm") %>%
    html_text() %>% 
    as.numeric() %>%
    na.omit() %>% 
    max()
  #####
  print(paste("FOUND", mp, "PAGES"))
  #####
  
  ##FOR LOOP FOR GETTING PAGES
  
  for(t in 2:mp){
    #nachwahl for pages
    
    #####
    print(paste("CHECKING PAGE", t, "OF", mp))
    #####
    
    #generated paginated link
    it.url <- paste(paste(Tr.burl, zips[i], znw, t, pnw, sep = ""))
    #grabbing each page's links
    slp <- sample(1:6, 1)
    print(paste("Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)
    
    #####
    print(paste("TRY CATCH FOR PAGE", t, "OF", mp))
    #####
    
    chka <-  tryCatch({
      getURL(it.url) %>%
        read_html()
    },
    error = function(e){e}
    )
    
    if(inherits(chka, "error")) {
      print("URL Broken... Trying Again")
      chka <-  tryCatch({
        getURL(it.url) %>%
          read_html()
      },
      error = function(e){e}
      )
      
      if(inherits(chka, "error")) {
        print("URL Broken")
        next
      }
    }
    
    #####
    print(paste("TRY CATCH FOR PAGE", t, "OF", mp, "SUCCESS"))
    #####
    
    slp <- sample(1:5, 1)
    print(paste("Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)
    
    #####
    print(paste("READING PAGE", t, "OF", mp))
    #####
    
    pll <- getURL(it.url) %>%
      read_html()
    mpl <- pll %>%
      html_nodes(".xsCol12Landscape.smlCol12.lrgCol8") %>%
      html_nodes(".card.boxBasic.backgroundBasic") %>%
      html_children() %>%
      html_children() %>%
      html_attr("href") %>% 
      na.omit() %>%
      as.character() %>%
      unique()
    pl <- c(pl, mpl)
    #####
    print(paste("++++ FINISHED PAGE", t, "OF", mp, "++++"))
    #####
    
    
  } #END OF PAGE LINK GRAB LOOP
  
  
  #Create dummy df to put info into
  trul <- data.frame(post_title = as.character(),
                     address = as.character(),
                     price = as.character(),
                     beds = as.character(),
                     baths = as.character(),
                     sqft = as.character(),
                     pets = as.character(),
                     desc = as.character(),
                     scrape_date = as.character(),
                     scrape_zip = as.character(),
                     link = as.character()
  )
  
  for (j in 1:length(pl)){
    #Generate Link URL
    lurl <- paste("https://www.trulia.com", pl[j], sep = "")
    
    print(paste("CHECKING FOR DUPLICATE URL"))
    if (lurl %in% curs$link){
      print(paste("URL", j, "EXISTS IN DATASET"))
      next
    }
    
    #Load Listing
    slp <- sample(1:6, 1)
    print(paste("Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)
    
    #####
    print(paste("TRY CATCH FOR LINK", j, "OF", length(pl), "FOR ZIP", i, "OF", length(zips)))
    #####
    
    chka <-  tryCatch({
      getURL(lurl) %>%
        read_html()
    },
    error = function(e){e}
    )
    
    if(inherits(chka, "error")) {
      print("URL Broken... Trying Again")
      chka <-  tryCatch({
        getURL(lurl) %>%
          read_html()
      },
      error = function(e){e}
      )
      
      if(inherits(chka, "error")) {
        print("----")
        print("URL Broken")
        next
      }
    }
    
    #####
    print(paste("TRY CATCH FOR LINK", j, "OF", length(pl), "SUCCESS FOR ZIP", i, "OF", length(zips)))
    #####
    
    slp <- sample(1:5, 1)
    print(paste("Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)
    
    lst <- getURL(lurl) %>%
      read_html()
    
    
    #####
    print(paste("LINK", j, "OF", length(pl), "READ FOR ZIP", i, "OF", length(zips)))
    #####
    
    ##FINDING NUM OF ALL LISTED APARTMENTS
    
    
    #####
    print(paste("CHECKING NUMBER OF UNITS FOR LINK", j, "OF", length(pl), "FOR ZIP", i, "OF", length(zips)))
    #####
    
    taps <- lst %>%
      html_nodes(".fpTableRow.pvn.mvn.prs.clickable") %>% 
      length()
    if (taps > 0){
      
      
      #####
      print(paste("FOUND", taps, "UNITS IN LINK", j, "OF", length(pl), "FOR ZIP", i, "OF", length(zips)))
      #####
      
      for (z in 1:taps){
        
        
        #####
        print(paste("GRABBING UNIT", z, "OF", taps, "FOR LINK", j, "OF", length(pl), "FOR ZIP", i, "OF", length(zips)))
        #####
        
        ##Post Title
        ptit <- lst %>%
          html_nodes(".h2.typeEmphasize.pan.man.defaultLineHeight") %>%
          html_children() %>%
          html_text()
        if(length(ptit) == 0){
          ptit <- NA
        }
        
        ##Adress
        padd <- lst %>%
          html_nodes(".h2.defaultLineHeight") %>%
          html_nodes(".main") %>%
          html_node(".h6.typeWeightNormal.pts.typeLowlight.xxsHidden") %>% 
          html_text() %>%
          gsub("\n", "",.) %>% 
          gsub("  ", "",.)
        if(length(padd) == 0){
          padd <- NA
        }
        
        ##Price
        tpr <- lst %>%
          html_nodes(".fpTableRow.pvn.mvn.prs.clickable") %>% 
          .[z] %>% 
          html_nodes(".fpCol45") %>%
          html_children() %>%
          html_children() %>%
          html_text() %>% 
          #From this point ^ the elements produced follow the following pattern:
          #1=the unit number, 2=bed/bath, 3=sqft, 4=price, 5=availability
          .[4]
        if(length(tpr) == 0){
          tpr <- NA
        }
        
        ##Num Bedrooms
        bds <- lst %>%
          html_nodes(".fpTableRow.pvn.mvn.prs.clickable") %>% 
          .[z] %>% 
          html_nodes(".fpCol45") %>%
          html_children() %>%
          html_children() %>%
          html_text() %>% 
          #From this point ^ the elements produced follow the following pattern:
          #1=the unit number, 2=bed/bath, 3=sqft, 4=price, 5=availability
          .[2] %>% 
          gsub(" bd.*", "", .)
        if(length(bds) == 0){
          bds <- NA
        }
        
        ##Num Baths
        bths <- lst %>%
          html_nodes(".fpTableRow.pvn.mvn.prs.clickable") %>% 
          .[z] %>% 
          html_nodes(".fpCol45") %>%
          html_children() %>%
          html_children() %>%
          html_text() %>% 
          #From this point ^ the elements produced follow the following pattern:
          #1=the unit number, 2=bed/bath, 3=sqft, 4=price, 5=availability
          .[2] %>% 
          gsub(".* bd", "", .) %>% 
          gsub(" ba.*", "", .)
        if(length(bths) == 0){
          bths <- NA
        }
        
        ##SQFT
        SQFT <- lst %>%
          html_nodes(".fpTableRow.pvn.mvn.prs.clickable") %>% 
          .[z] %>% 
          html_nodes(".fpCol45") %>%
          html_children() %>%
          html_children() %>%
          html_text() %>% 
          #From this point ^ the elements produced follow the following pattern:
          #1=the unit number, 2=bed/bath, 3=sqft, 4=price, 5=availability
          .[3]
        if(length(SQFT) == 0){
          SQFT <- NA
        }
        
        ##Pets
        pets <-  lst %>%
          html_nodes(".row.mbm") %>% 
          html_nodes(".iconDog") %>%
          html_text() %>%
          gsub("\n", "",.) %>% 
          gsub("  ", "",.)
        if(length(pets) == 0){
          pets <- NA
        }
        
        ##Description
        desc <-  lst %>%
          html_nodes("#propertyDescription") %>% 
          html_text() %>%
          gsub("\n", "",.) %>% 
          gsub("  ", "",.)
        if(length(desc == 0)){
          desc <- NA
        }
        
        gtrul <- data.frame(post_title = ptit,
                            address = padd,
                            price = tpr,
                            beds = bds,
                            baths = bths,
                            sqft = SQFT,
                            pets = pets,
                            desc = desc,
                            scrape_date = Sys.time(),
                            scrape_zip = zips[i],
                            link = lurl)
        trul <- rbind(trul, gtrul)
        
        #####
        print(paste("++++","FINISHED UNIT", z, "OF", taps, "FOR LINK", j, "OF", length(pl), "FOR ZIP", i, "OF", length(zips),"++++"))
        #####
        
      } #END MULTI APARTMENT LISTING LOOP
      
      #####
      print(paste("FINISHED LINK", j, "OF", length(pl), "FOR ZIP", i, "OF", length(zips)))
      #####
      
    } else {     #END IF STATEMENT TO CATCH MULTIPLE APARTMENTS
      
      
      #####
      print(paste("STARTING LINK", j, "OF", length(pl), "FOR ZIP", i, "OF", length(zips)))
      #####
      
      ##Post Title
      ptit <- lst %>%
        html_nodes(".h2.typeEmphasize.pan.man.defaultLineHeight") %>%
        html_children() %>%
        html_text()
      if(length(ptit) == 0){
        ptit <- NA
      }
      
      ##Adress
      padd <- lst %>%
        html_nodes(".h2.defaultLineHeight") %>%
        html_nodes(".main") %>%
        html_node(".h6.typeWeightNormal.pts.typeLowlight.xxsHidden") %>% 
        #html_children() %>%
        html_text() %>%
        gsub("\n", "",.) %>% 
        gsub("  ", "",.)
      if(length(padd) == 0){
        padd <- NA
      }
      
      ##Price
      tpr <- lst %>%
        html_nodes(".mvn") %>% 
        html_nodes(".h3.typeEmphasize") %>%
        html_text() %>% 
        gsub("\n", "",.) %>% 
        gsub("  ", "",.)
      if(length(tpr) == 0){
        tpr <- NA
      }
      
      ##Num Bedrooms
      bds <- lst %>%
        html_nodes(".listInlineBulleted.man.pts#property_features") %>% 
        html_children() %>%
        .[grep("beds", .)] %>%
        html_text() %>%
        gsub(" beds", "", .)
      if(length(bds) == 0){
        bds <- NA
      }
      
      ##Num Baths
      bths <- lst %>%
        html_nodes(".listInlineBulleted.man.pts#property_features") %>% 
        html_children() %>%
        .[grep("baths", .)] %>%
        html_text() %>%
        gsub(" baths", "", .)
      if(length(bths) == 0){
        bths <- NA
      }
      
      ##SQFT
      SQFT <- lst %>%
        html_nodes(".listInlineBulleted.man.pts#property_features") %>% 
        html_children() %>%
        .[grep("sqft", .)] %>%
        html_text() %>%
        gsub(" sqft", "", .)
      if(length(SQFT) == 0){
        SQFT <- NA
      }
      
      ##Pets
      pets <-  lst %>%
        html_nodes(".row.mbm") %>% 
        html_nodes(".iconDog") %>%
        html_text() %>%
        gsub("\n", " ",.) %>% 
        gsub("  ", "",.)
      if(length(pets) == 0){
        pets <- NA
      }
      
      ##Description
      desc <-  lst %>%
        html_nodes("#propertyDescription") %>% 
        html_text() %>%
        gsub("\n", "",.) %>% 
        gsub("  ", "",.)
      if(length(desc) == 0){
        desc <- NA
      }
      
      gtrul <- data.frame(post_title = ptit,
                          address = padd,
                          price = tpr,
                          beds = bds,
                          baths = bths,
                          sqft = SQFT,
                          pets = pets,
                          desc = desc,
                          scrape_date = Sys.time(),
                          scrape_zip = zips[i],
                          link = lurl)
      trul <- rbind(trul, gtrul)
      
      #####
      print(paste("++++","FINISHED LINK", j, "OF", length(pl), "FOR ZIP", i, "OF", length(zips), "++++"))
      #####
      
    } #END OF ELSE STATEMENT THAT ASSUMES THERE IS JUST ONE APARTMENT LISTED IN THIS LINK
  } #END OF ALL LINKS LOOP
  #putting the scraped links together for the entire zip
  mtr <- rbind(mtr, trul)
  
  #####
  print(paste("++++", "FINISHED ZIP", i, "OF", length(zips), "++++"))
  #####
} #END OF ZIPS LOOP

mtr <- unique(mtr)
write_civis(mtw, tablename = "sandbox.trulia_master", if_exists = "append")


