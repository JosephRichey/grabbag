
# Private helper function
GenerateSensitivityVector <- function(base, numSteps, stepSize) {
  names <- c(seq((numSteps * stepSize) + base,
                 base,
                 -stepSize),
             seq(base - stepSize,
                 -(numSteps * stepSize) + base,
                 -stepSize))

  if(length(names) != numSteps * 2 + 1) {
    stop("The base paratmerts are too big and step size is too small. Please adjust your inputs.")
  }

  return(names)
}

# Private helper function
GenerateSensitivityValidateInputs <- function(tableFormula,
                                              var1Base,
                                              var2Base,
                                              var1NumSteps,
                                              var2NumSteps,
                                              var1StepSize,
                                              var2StepSize,
                                              nDigits,
                                              valueFormat,
                                              returnTable) {
  # Validate inputs
  ## Check that formula is passed
  if (missing(tableFormula)) {
    stop("Please provide a valid formula.")
  }

  ## Check that formula has only two inputs.
  arguments <- names(formals(tableFormula))
  if (length(arguments) != 2) {
    stop("The formula must have exactly two variables.")
  }

  ## Check that the formula evalutes to a number. If it doesn't, throw a warning.
  if(is.na(tableFormula(var1Base, var2Base))) {
    warning(paste("The formula you provided results in a NA value with ",
                  var1Base, "and", var2Base, "as inputs. You may want to check
                  your base inputs or formula."))
  }

  ## If the formula results in an error, throw an error.
  tryCatch(
    {
      tableFormula(var1Base, var2Base)
    },
    error = function(e) {
      stop(paste("The formula you provided does not evaluate with", var1Base,
                 "and", var2Base, "as inputs. Check your base inputs or formula."))
    }
  )


  ## Check that inputs are numeric and in proper range
  if (!all(is.numeric(var1Base),
           is.numeric(var2Base),
           is.numeric(var1StepSize),
           is.numeric(var2StepSize))) {
    stop("Please ensure you are passing numbers for base and size.")
  }

  if(!all(gmp::is.whole(var1NumSteps),
          gmp::is.whole(var2NumSteps),
          gmp::is.whole(nDigits))) {
    stop("Please ensure you are passing integers for steps and nDigits.")
  }

  if(!all(var1NumSteps > 0,
          var2NumSteps > 0)) {
    stop("Please ensure all size parameters are greater that zero.")
  }

  if(!is.logical(returnTable)) {
    stop("Please ensure you are passing a logical value for returnTable.")
  }

  if(!(valueFormat %in% c('number', 'percent'))) {
    stop(paste0("Your input of ", valueFormat, " is not number or percent. Please enter a valid argument."))
  }
}

#' Generate a sensitivity table given a two input formula.
#'
#' @param tableFormula A formula with two inputs that will define the sensitivity table values.
#' @param var1Base The base value of the first input of the formula.
#' @param var2Base The base value of the second input of the formula.
#' @param var1StepSize The step size of the first input of the formula. A negative value flips the order of values.
#' @param var2StepSize The step size of the second input of the formula. A negative value flips the order of values.
#' @param var1NumSteps The number of steps above and below the first input base.
#' @param var2NumSteps The number of steps above and below the second input base.
#' @param nDigits The number of digits the values will round to.
#' @param valueFormat What format the values should be. Options are "number" and "percent".
#' @param returnTable Whether a DT table should be returned or a matrix.
#'
#'
#' @return A DT sensitivity table, or a matrix depending on returnTable.
#' @export
#'
#' @examples
#' #Generate a sensitivity table for monthly mortgage payments of a 30 year mortgage
#' #based on the principle and interest rate.
#'
#' pmt <- function(prinical, rate) {
#'   prinical * (rate/12)/(1-(1+rate/12)^-360)
#' }
#'
#' GenerateSensitivityTable(pmt, 340000, .06, 10000, .0025)
#' GenerateSensitivityTable(pmt, 340000, .06, 10000, .0025, 5, 5, 0, 'number', FALSE)
#'
GenerateSensitivityTable <- function(tableFormula,
                                     var1Base = 0,
                                     var2Base = 0,
                                     var1StepSize = 1,
                                     var2StepSize = 1,
                                     var1NumSteps = 3,
                                     var2NumSteps = 3,
                                     nDigits = 0,
                                     valueFormat = 'number',
                                     returnTable = TRUE) {

  # Validate Inputs
  GenerateSensitivityValidateInputs(tableFormula,
                                    var1Base,
                                    var2Base,
                                    var1NumSteps,
                                    var2NumSteps,
                                    var1StepSize,
                                    var2StepSize,
                                    nDigits,
                                    valueFormat,
                                    returnTable)

  # Generate matrix for values
  my_matrix <- matrix(nrow = var1NumSteps * 2 + 1,
                      ncol = var2NumSteps * 2 + 1)


  rownames(my_matrix) <- GenerateSensitivityVector(var1Base, var1NumSteps, var1StepSize)

  colnames(my_matrix) <- GenerateSensitivityVector(var2Base, var2NumSteps, var2StepSize)


  my_matrix

  if(nDigits == 0) {
    desired_accuracy <- 1
  } else {
    desired_accuracy <- as.numeric(paste0("0.",paste0(rep(0, nDigits - 1),collapse = ""), "1"))
  }

  # Fill values
  for(i in 1:nrow(my_matrix)) {
    for (j in 1:ncol(my_matrix)) {
      if(valueFormat == 'percent') {
        my_matrix[i,j] <- scales::percent(tableFormula(as.numeric(rownames(my_matrix)[i]),
                                                        as.numeric(colnames(my_matrix)[j])), accuracy = desired_accuracy)
      } else {
        my_matrix[i,j] <- round(tableFormula(as.numeric(rownames(my_matrix)[i]),
                                              as.numeric(colnames(my_matrix)[j])), nDigits)

      }
    }
  }



  # Reurn desired output
  if(returnTable) {
    DT::datatable(my_matrix)
  } else {
    my_matrix
  }
}
