#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <fes.h>


#define MAX_HANDLES 127

static unsigned int num_hdls = 0;
static FES open_hdls[MAX_HANDLES + 1];

// unix epoch to fes2014 epoch: 1970/01/01 00:00:00 - 1950/01/01 00:00:00
static const double epoch_offset = (2440587.5 - 2433282.5);

typedef struct {
	int debug_handle;
	int debug_calculate;
} Options;
static Options options = { 0, 0 };


const char const * const _err_string(int errno) {
	switch(errno) {
		case -1:
			return "Not enough memory";
		case -2:
			return "netCDF error";
		case -3:
			return "IO error";
		case -4:
			return "Configuration file contains error";
		case -5:
			return "Tide is undefined (no data)";
		case -6:
			return "Value error (invalid argument)";
		case 0:
		default:
			return "Success";
	}
}


void fes2014_debugHandles(SEXP r_enable) {
	if (Rf_asLogical(r_enable)) {
		options.debug_handle = 1;
	} else {
		options.debug_handle = 0;
	}
}
SEXP fes2014_isDebugHandlesEnabled() {
	return Rf_ScalarLogical(options.debug_handle);
}

void fes2014_debugCalculate(SEXP r_enable) {
	if (Rf_asLogical(r_enable)) {
		options.debug_calculate = 1;
	} else {
		options.debug_calculate = 0;
	}
}
SEXP fes2014_isDebugCalculateEnabled() {
	return Rf_ScalarLogical(options.debug_calculate);
}


static void _fes2014_finalize(SEXP r_hdl);

SEXP fes2014_new(SEXP r_tideType, SEXP r_accessMode, SEXP r_iniFile) {
	FES fes_hdl;

	if (num_hdls == MAX_HANDLES) {
		Rf_error("Too many FES handlers opened");
		return R_NilValue;
	}

	int res = fes_new(
		&fes_hdl,
		(fes_enum_tide_type)Rf_asInteger(r_tideType),
		(fes_enum_access)Rf_asInteger(r_accessMode),
		Rf_translateCharUTF8(Rf_asChar(r_iniFile))
	);

	if (res == 1) {
		Rf_error("Not enough memory to allocate the FES handler");
		return R_NilValue;
	}

	*(open_hdls + num_hdls++) = fes_hdl;
	if (options.debug_handle) {
		Rprintf("Created FES handle: %08x\n", fes_hdl);
	}

	SEXP r_hdl = R_MakeExternalPtr((void *)fes_hdl, Rf_install("Rfes2014_ptr"), R_NilValue);
    Rf_protect(r_hdl);
    R_RegisterCFinalizerEx(r_hdl, _fes2014_finalize, TRUE);
    Rf_unprotect(1);

	return r_hdl;
}

void fes2014_delete(SEXP r_hdl) {
	FES fes_hdl = (FES)R_ExternalPtrAddr(r_hdl);

	for (int ii = 0; ii < num_hdls; ii++) {
		if (*(open_hdls + ii) == fes_hdl) {
			for (int jj = ii; jj < (num_hdls - 1); jj++) {
				*(open_hdls + jj) = *(open_hdls + jj + 1);
			}
			num_hdls--;
			break;
		}
	}

	if (options.debug_handle) {
		Rprintf("Destroying FES handle: %08x\n", fes_hdl);
	}
	fes_delete(fes_hdl);

	R_ClearExternalPtr(r_hdl);
}

static void _fes2014_finalize(SEXP r_hdl) {
    if(!R_ExternalPtrAddr(r_hdl)) return;

	FES fes_hdl = (FES)R_ExternalPtrAddr(r_hdl);

	if (options.debug_handle) {
		Rprintf("Finalizing handle: %08x\n", fes_hdl);
	}
    fes2014_delete(r_hdl);
}


SEXP fes2014_calculate_one(SEXP r_hdl, SEXP r_lat, SEXP r_lon, SEXP r_epochSec) {
	FES fes_hdl = (FES)R_ExternalPtrAddr(r_hdl);

	double lat = Rf_asReal(r_lat);
	double lon = Rf_asReal(r_lon);
	double julianTs = (Rf_asInteger(r_epochSec) / 86400.0) + epoch_offset;
	double h = NA_REAL, h_long_period = NA_REAL;
	int err, min_num_points = NA_INTEGER;

	if (options.debug_calculate) {
		Rprintf("Calculating for: %f %f %f\n", lat, lon, julianTs);
	}

	int res = fes_core(fes_hdl, lat, lon, julianTs, &h, &h_long_period);
	if (res == 0) {
		min_num_points = fes_min_number(fes_hdl);

		if (options.debug_calculate) {
			Rprintf("Result: %f %f %d\n", h, h_long_period, min_num_points);
		}
	} else {
		err = (int)fes_errno(fes_hdl);
		if (options.debug_calculate) {
			Rprintf("Error: %s\n", _err_string(-err));
		}

		if (err == (int)FES_NO_DATA) {
			h = NA_REAL;
			h_long_period = NA_REAL;
		} else {
			Rf_error(_err_string(-err));
		}
	}

	SEXP r_h = Rf_protect(Rf_ScalarReal(h));
	SEXP r_hLongPeriod = Rf_protect(Rf_ScalarReal(h_long_period));
	SEXP r_samples = Rf_protect(Rf_ScalarInteger(min_num_points));

	SEXP r_resultVec = Rf_protect(Rf_allocVector(VECSXP, 3));
	SET_VECTOR_ELT(r_resultVec, 0, r_h);
	SET_VECTOR_ELT(r_resultVec, 1, r_hLongPeriod);
	SET_VECTOR_ELT(r_resultVec, 2, r_samples);

	SEXP r_resultNames = Rf_protect(Rf_allocVector(STRSXP, 3));
	SET_STRING_ELT(r_resultNames, 0, Rf_mkChar("h"));
	SET_STRING_ELT(r_resultNames, 1, Rf_mkChar("hLongPeriod"));
	SET_STRING_ELT(r_resultNames, 2, Rf_mkChar("samples"));

	Rf_setAttrib(r_resultVec, R_NamesSymbol, r_resultNames);

	Rf_unprotect(5);
	return r_resultVec;
}

SEXP fes2014_calculate_many(SEXP r_hdl, SEXP r_lat, SEXP r_lon, SEXP r_epochSec, SEXP r_nRow) {
	FES fes_hdl = (FES)R_ExternalPtrAddr(r_hdl);

	int nrow = Rf_asInteger(r_nRow);

	double *latPtr = REAL(r_lat);
	double *lonPtr = REAL(r_lon);
	int *epochSecPtr = INTEGER(r_epochSec);

	SEXP r_hVec = Rf_protect(Rf_allocVector(REALSXP, nrow));
	SEXP r_hLongPeriodVec = Rf_protect(Rf_allocVector(REALSXP, nrow));
	SEXP r_samplesVec = Rf_protect(Rf_allocVector(INTSXP, nrow));

	double *hVecPtr = REAL(r_hVec);
	double *hLongPeriodVecPtr = REAL(r_hLongPeriodVec);
	int *samplesVecPtr = INTEGER(r_samplesVec);

	if (options.debug_calculate) {
		Rprintf("Processing %d rows\n", nrow);
	}

	int res, err, ii;
	double julianTs;
	for (ii = 0; ii < nrow; ii++) {
		julianTs = (*(epochSecPtr + ii) / 86400.0) + epoch_offset;

		if (options.debug_calculate) {
			Rprintf(
				"\t[%d] Calculating for: %f %f %f\n",
				ii,
				*(latPtr + ii),
				*(lonPtr + ii),
				julianTs
			);
		}

		res = fes_core(
			fes_hdl,
			*(latPtr + ii),
			*(lonPtr + ii),
			julianTs,
			hVecPtr + ii,
			hLongPeriodVecPtr + ii
		);

		if (res == 0) {
			*(samplesVecPtr + ii) = fes_min_number(fes_hdl);

			if (options.debug_calculate) {
				Rprintf(
					"\t[%d] Result: %f %f %d\n",
					ii,
					*(hVecPtr + ii),
					*(hLongPeriodVecPtr + ii),
					*(samplesVecPtr + ii)
				);
			}
		} else {
			err = (int)fes_errno(fes_hdl);

			if (options.debug_calculate) {
				Rprintf("\t[%d] Error: %s\n", ii, _err_string(-err));
			}

			if (err == (int)FES_NO_DATA) {
				*(hVecPtr + ii) = NA_REAL;
				*(hLongPeriodVecPtr + ii) = NA_REAL;
				*(samplesVecPtr + ii) = NA_INTEGER;
			} else {
				Rf_error(_err_string(-err));
			}
		}
	}

	SEXP r_resultVec = Rf_protect(Rf_allocVector(VECSXP, 3));
	SET_VECTOR_ELT(r_resultVec, 0, r_hVec);
	SET_VECTOR_ELT(r_resultVec, 1, r_hLongPeriodVec);
	SET_VECTOR_ELT(r_resultVec, 2, r_samplesVec);

	SEXP r_resultNames = Rf_protect(Rf_allocVector(STRSXP, 3));
	SET_STRING_ELT(r_resultNames, 0, Rf_mkChar("h"));
	SET_STRING_ELT(r_resultNames, 1, Rf_mkChar("hLongPeriod"));
	SET_STRING_ELT(r_resultNames, 2, Rf_mkChar("samples"));

	Rf_setAttrib(r_resultVec, R_NamesSymbol, r_resultNames);

	Rf_unprotect(5);
	return r_resultVec;
}

