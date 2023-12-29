
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
GenerateSensitivityValidateInputs <- function(table_formula,
                                              var1Base,
                                              var2Base,
                                              var1NumSteps,
                                              var2NumSteps,
                                              var1StepSize,
                                              var2StepSize,
                                              nDigits,
                                              value_format,
                                              returnTable) {
  # Validate inputs
  ## Check that formula is passed
  if (missing(table_formula)) {
    stop("Please provide a valid formula.")
  }

  ## Check that formula has only two inputs.
  arguments <- names(formals(table_formula))
  if (length(arguments) != 2) {
    stop("The formula must have exactly two variables.")
  }

  ## Check that the formula evalutes to a number. If it doesn't, throw a warning.
  if(is.na(table_formula(var1Base, var2Base))) {
    warning(paste("The formula you provided results in a NA value with ",
                  var1Base, "and", var2Base, "as inputs. You may want to check
                  your base inputs or formula."))
  }

  ## If the formula results in an error, throw an error.
  tryCatch(
    {
      table_formula(var1Base, var2Base)
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

  if(!(value_format %in% c('number', 'percent'))) {
    stop(paste0("Your input of ", value_format, " is not number or percent. Please enter a valid argument."))
  }
}


GenerateSensitivityTable <- function(table_formula,
                                     var1Base = 0,
                                     var2Base = 0,
                                     var1StepSize = 1,
                                     var2StepSize = 1,
                                     var1NumSteps = 3,
                                     var2NumSteps = 3,
                                     nDigits = 0,
                                     value_format = 'number',
                                     returnTable = TRUE) {

  # Validate Inputs
  GenerateSensitivityValidateInputs(table_formula,
                                    var1Base,
                                    var2Base,
                                    var1NumSteps,
                                    var2NumSteps,
                                    var1StepSize,
                                    var2StepSize,
                                    nDigits,
                                    value_format,
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
      if(value_format == 'percent') {
        my_matrix[i,j] <- scales::percent(table_formula(as.numeric(rownames(my_matrix)[i]),
                                                        as.numeric(colnames(my_matrix)[j])), accuracy = desired_accuracy)
      } else {
        my_matrix[i,j] <- round(table_formula(as.numeric(rownames(my_matrix)[i]),
                                              as.numeric(colnames(my_matrix)[j])), nDigits)

      }
    }
  }



  # Reurn desired output
  if(returnTable) {
    DT:::datatable(my_matrix)
  } else {
    my_matrix
  }
}
