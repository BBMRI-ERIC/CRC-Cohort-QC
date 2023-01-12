# Function

getStage <- function(pT, pN, pM, uiccVersion) {
	if (uiccVersion == "6th edition") {
		if (pM == "M1") { return("IV") }
		else if (pM == "MX") { return(NA) }
		else if (pM == "M0") {
			if (pT == "Tis" && pN == "N0") { return("0") }
			else if ((pT == "T1" || pT == "T2") && pN == "N0") { return("I") }
			else if ((pT == "T3" ) && pN == "N0" ) { return("IIA") } 
			else if ((pT == "T4" ) && pN == "N0" ) { return("IIB") }
			else if ((pT == "T1" || pT == "T2" ) && pN == "N1" ) { return("IIIA") }
			else if ((pT == "T3" || pT == "T4" ) && pN == "N1" ) { return("IIIB") }
			else if ( pN == "N2" ) { return("IIIC") }
			else {
				warning(paste("Unknown combination of pTN values in TNM 6th ed: pT = ", pT, ", pN = ", pN, sep=""))
			}
		}
		else {
			warning(paste("Unknown M value for TNM 6th ed: pM = ", pM, sep=""))
		}
	}
	else if (uiccVersion == "7th edition") {
		if (pM == "M1") { return("IV") }
		else if (pM == "M1a") { return("IVA") }
		else if (pM == "M1b") { return("IVB") }
		else if (pM == "M0") {
			if (pT == "Tis" && pN == "N0" ) { return("0") }
			else if ((pT == "T1" || pT == "T2" ) && pN == "N0" ) { return("I") }
			else if ((pT == "T3" ) && pN == "N0" ) { return("IIA") }
			else if ((pT == "T4a" ) && pN == "N0" ) { return("IIB") }
			else if ((pT == "T4b" ) && pN == "N0" ) { return("IIC") }
			else if ((pT == "T4" ) && pN == "N0" ) { return("II") }
			else if ((pT == "T1" || pT == "T2" ) && (pN == "N1" || pN == "N1a" || pN == "N1b" || pN == "N1c" )) { return("IIIA") }
			else if ((pT == "T1") && pN == "N2a" ) { return("IIIA") }
			else if ((pT == "T3" || pT == "T4a" ) && (pN == "N1" || pN == "N1a" || pN == "N1b" || pN == "N1c" )) { return("IIIB") }
			else if ((pT == "T2" || pT == "T3" ) && pN == "N2a" ) { return("IIIB") }
			else if ((pT == "T1" || pT == "T2" ) && pN == "N2b" ) { return("IIIB") }
			else if ((pT == "T4a") && pN == "N2a" ) { return("IIIC") }
			else if ((pT == "T3" || pT == "T4a" ) && pN == "N2b" ) { return("IIIC") }
			else if ((pT == "T4b" ) && (pN == "N1" || pN == "N1a" || pN == "N1b" || pN == "N1c"  || pN == "N2" || pN == "N2a" || pN == "N2b")) { return("IIIC") }
			else if ( (pN == "N1" || pN == "N1a" || pN == "N1b" || pN == "N1c" || pN == "N2" || pN == "N2a" || pN == "N2b")) { return("III") }
			else { 
				warning(paste("Unknown combination of pTN values in TNM 7th ed: pT = ", pT, ", pN = ", pN, sep=""))
			}
		}
		else {
			warning(paste("Unknown M value in TNM 7th ed: ", pM, sep=""))
		}
	}
	return(NA)
}

getValidSurgeries <- function(loc, stype) {
	# surgery location & surgery type
	if (loc == "C18.0" || loc == "C18.1" || loc == "C18.2") {
		if (stype == "Right hemicolectomy" || stype == "Other") { return(NA) }
		else if (stype == "Pan-procto colectomy" || stype == "Total colectomy") { return(paste0("Suspect combination of surgery location ", loc, " and surgery type ", stype)) }
		else { return(paste0("Invalid combination of surgery location ", loc, " and surgery type ", stype)) }
	}
	else if (loc == "C18.3") {
		if (stype == "Right hemicolectomy" || stype == "Other") { return(NA) }
		else if (stype == "Pan-procto colectomy" || stype == "Total colectomy" || stype == "Transverse colectomy") { return(paste0("Suspect combination of surgery location ", loc, " and surgery type ", stype)) }
		else { return(paste0("Invalid combination of surgery location ", loc, " and surgery type ", stype)) }
	}
	else if (loc == "C18.4") {
		if (stype == "Transverse colectomy" || stype == "Other") { return(NA) }
		else if (stype == "Left hemicolectomy" || stype == "Pan-procto colectomy" || stype == "Right hemicolectomy" || stype == "Total colectomy") { return(paste0("Suspect combination of surgery location ", loc, " and surgery type ", stype)) }
		else { return(paste0("Invalid combination of surgery location ", loc, " and surgery type ", stype)) }
	}
	else if (loc == "C18.5") {
		if (stype == "Left hemicolectomy" || stype == "Other") { return(NA) }
		else if (stype == "Abdomino-perineal resection" || stype == "Pan-procto colectomy" || stype == "Total colectomy" || stype == "Sigmoid colectomy" || stype == "Transverse colectomy") { return(paste0("Suspect combination of surgery location ", loc, " and surgery type ", stype)) }
		else { return(paste0("Invalid combination of surgery location ", loc, " and surgery type ", stype)) }
	}
	else if (loc == "C18.6") {
		if (stype == "Left hemicolectomy" || stype == "Other") { return(NA) }
		else if (stype == "Abdomino-perineal resection" || stype == "Pan-procto colectomy" || stype == "Total colectomy" || stype == "Sigmoid colectomy") { return(paste0("Suspect combination of surgery location ", loc, " and surgery type ", stype)) }
		else { return(paste0("Invalid combination of surgery location ", loc, " and surgery type ", stype)) }
	}
	else if (loc == "C18.7") {
		if (stype == "Sigmoid colectomy" || stype == "Left hemicolectomy" || stype == "Other") { return(NA) }
		else if (stype == "Abdomino-perineal resection" || stype == "Low anteroir colon resection" || stype == "Pan-procto colectomy" || stype == "Total colectomy" ) { return(paste0("Suspect combination of surgery location ", loc, " and surgery type ", stype)) }
		else { return(paste0("Invalid combination of surgery location ", loc, " and surgery type ", stype)) }
	}
	# this shoud not occur in CRC-cohort but we keep it for sake of completeness
	else if (loc == "C18.8") {
		# all is permitted
		return (NA)
	}
	# this shoud not occur in CRC-cohort but we keep it for sake of completeness
	else if (loc == "C18.9") {
		if (stype == "Left hemicolectomy" || stype == "Pan-procto colectomy" || stype == "Right hemicolectomy" || stype == "Sigmoid colectomy" || stype == "Total colectomy" || stype == "Transverse colectomy" || stype == "Other" ) { return(NA) }
		else if (stype == "Abdomino-perineal resection" || stype == "Low anteroir colon resection") { return(paste0("Suspect combination of surgery location ", loc, " and surgery type ", stype)) }
		else { return(paste0("Invalid combination of surgery location ", loc, " and surgery type ", stype)) }
	}
	else if (loc == "C19" || loc == "C19.9") {
		if (stype == "Anterior resection of rectum" || stype == "Endo-rectal tumor resection" || stype == "Low anteroir colon resection" || stype == "Sigmoid colectomy" || stype == "Other") { return(NA) }
		else if (stype == "Abdomino-perineal resection" || stype == "Left hemicolectomy" || stype == "Pan-procto colectomy" || stype == "Total colectomy") { return(paste0("Suspect combination of surgery location ", loc, " and surgery type ", stype)) }
		else { return(paste0("Invalid combination of surgery location ", loc, " and surgery type ", stype)) }
	}
	else if (loc == "C20" || loc == "C20.9") {
		if (stype == "Abdomino-perineal resection" || stype == "Anterior resection of rectum" || stype == "Endo-rectal tumor resection" || stype == "Low anteroir colon resection" || stype == "Other") { return(NA) }
		else if (stype == "Left hemicolectomy" || stype == "Pan-procto colectomy" || stype == "Total colectomy") { return(paste0("Suspect combination of surgery location ", loc, " and surgery type ", stype)) }
		else { return(paste0("Invalid combination of surgery location ", loc, " and surgery type ", stype)) }
	}
	return(NA)
}

