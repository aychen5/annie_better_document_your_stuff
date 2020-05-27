///////////////////////////////////////////////////////
/// 												///
/// rdbound: Calculate bounds on incumbency effects ///
//           conditional on candidacy				///
///          Based on Anagol and Fujiwara (2016)	///
///													///
/// Date: December 2016								///
/// Creator: BK Song 								///
/// 												///
///////////////////////////////////////////////////////

cap program drop rdbound

program define rdbound, eclass
	set more off
	version 13
	
	syntax varlist(min=4) [if] [in] [, bwselect(string) kernel(string) CLuster(string) h_run(real 0) h_win(real 0)]
	
	///////////////////////////////////////////////////////////////////////////////////////////////////
	/// varlist: first = candidate running, second = candidate wins									///
	///          third = treatment, fourth = running variable										///
	/// h_run, h_win: specifies the main bandwidth to be used to construct the RD point estimators  ///
	///               if not specified, this is computed by rdbwselect command                   	///
	///////////////////////////////////////////////////////////////////////////////////////////////////
	
	tempname b V m_est
	
	marksample touse
	
	if "`kernel'" == "" {
		local kernel = "triangular"
	}
	else {
		local kernel = lower("`kernel'")
	}
	
	if "`bwselect'" == "" {
		local bwselect = "CCT"
	}
	else {
		local bwselect = upper("`bwselect'")
	}
	
	preserve
	qui keep if `touse'
	
	tokenize `varlist'
	local run      `1'
	local win      `2'
	local treat    `3'
	local running  `4'
	local h_`1' = `h_run'
	local h_`2' = `h_win'
	
	* bandwidth selection and rd estimation
	
	eststo clear
	
	foreach y in `run' `win' {
	
		if `h_`y'' != 0 {
			local bw_`y' = `h_`y''
		}
		else {
			quietly rdbwselect_old `y' `running', bwselect(`bwselect') kernel(`kernel')
			local bw_`y' = e(h_`bwselect')
		}
		eststo rdd_`y': quietly reg `y' `treat'##c.`running' if abs(`running') < `bw_`y''
		local N_`y'  = e(N)
		local b_`y'  = _b[1.`treat']
		local se_`y' = _se[1.`treat']
	}
	
	if "`cluster'" == "" {
		quietly suest rdd_`run' rdd_`win'
	}
	else {
		quietly suest rdd_`run' rdd_`win', cl(`cluster')
	}	
	
	* estimates
	
	local treat_win "[rdd_`win'_mean]1.`treat'"
	local treat_run "[rdd_`run'_mean]1.`treat'"
	local R0        "[rdd_`run'_mean]_cons"
	local R1        "(`R0' + `treat_run')"
	local W0        "[rdd_`win'_mean]_cons/`R0'"
	local W1        "( [rdd_`win'_mean]_cons + [rdd_`win'_mean]1.`treat') / `R1' "
	
	local w1 = ( [rdd_`win'_mean]_cons + [rdd_`win'_mean]1.`treat') / `R1'
	local r1 = (`R0' + `treat_run')
	
	* upper bound estimate
	quietly nlcom (`treat_win' / `R1' ) * 100
	matrix `m_est' = [r(b), r(V)]
	
	* lower bound estimate
	quietly nlcom (`treat_win' - `treat_run' * `W1' ) / `R1' * 100
	matrix `m_est' = [`m_est' \ r(b), r(V)]
	
	* estiamte using W1/2
	quietly nlcom (`treat_win' - `treat_run' * `W1' / 2 ) / `R1' * 100
	matrix `m_est' = [`m_est' \ r(b), r(V)]
	
	
	local b_ub   = `m_est'[1,1]
	local b_lb   = `m_est'[2,1]
	local b_lb2  = `m_est'[3,1]
	local se_ub  = sqrt(`m_est'[1,2])
	local se_lb  = sqrt(`m_est'[2,2])
	local se_lb2 = sqrt(`m_est'[3,2])
	
	display ""
	display "<Effect on Running>"
	display "Coef.: " %5.3f `b_`run'' "  Std. Err.: " %5.3f `se_`run'' "  Bandwidth: " %5.3f `bw_`run'' "  N: " %5.0f `N_`run''
	display ""
	display "<Effect on Winning>"
	display "Coef.: " %5.3f `b_`win'' "  Std. Err.: " %5.3f `se_`win'' "  Bandwidth: " %5.3f `bw_`win'' "  N: " %5.0f `N_`win''
	display ""
	display "<Upper bound>"
	display "Coef.: " %5.3f  `b_ub' "  Std. Err.: " %5.3f  `se_ub'
	display ""
	display "<Lower bound>"
	display "Coef.: " %5.3f  `b_lb' "  Std. Err.: " %5.3f  `se_lb'
	display ""
	display "<Lower bound 2>"
	display "Coef.: " %5.3f  `b_lb2' "  Std. Err.: " %5.3f  `se_lb2'
	
		
	
	
	* ereturn
		
	ereturn clear
	foreach x in b_ub b_lb b_lb2 se_ub se_lb se_lb2 w1 r1 {
		ereturn scalar `x' = ``x''
	}
	foreach x in run win {
		ereturn scalar bw_`x' = `bw_``x'''
		ereturn scalar N_`x'  = `N_``x'''
		ereturn scalar b_`x'  = `b_``x'''
		ereturn scalar se_`x' = `se_``x'''
	}


end
