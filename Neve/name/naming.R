naming <- function(nametype){
  cat("\n\nWhen naming, here are some things to be aware of.
The name should:
  - Be one word (no spaces)
  - Be concise and descriptive of the dataframe
  - Start with a letter or a period (.) -- should not start with a number or symbol other than a period (.)
  - Avoid using punctuation other than a period (.), dash (-), or underscore (_)
  - Avoid using a name that is already being used")
    
  examples <- readline("If you would like to see examples of names, type YES, otherwise type anything or hit the return button: ")
  if(nametype == "df"){
    back(emamples, df_select())
  }
  if(nametype == "var"){
    
  }
  if(examples == "YES"){
    if(nametype == "df"){
      cat("\nLet's say you want to name a dataframe that contains info on 100 patients at a hospital

Good examples:
  - patients
  - .HospitalPatients
  - patients100
  - patients_df

Bad examples:
  - 100 patients at a hospital
  - data
  - dataframe_for_100_patients_at_hospital
  - _Patients
  - patients??
  - 100patients\n")}
    if(nametype == "var"){
      cat("\nLet's say you want to name a variable that shows the sex of a patient at a hospital

Good examples:
  - sex
  - PatientSex
  - .sex2
  - patients_df

Bad examples:
  - sex patients at a hospital
  - variable
  - variable_for_sex_of_patient
  - _sex
  - sex??
  - 2sex\n")}
  }
  if(nametype == "df"){
    return(readline("Enter what you would like to name the dataframe: "))}
  if(nametype == "var"){
    return(readline("Enter what you would like to name the variable: "))}
}

