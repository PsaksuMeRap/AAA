\name{available.desired.dataFrames}
\title{Dataframes for GUI construction}
\description{
The input of this function are two data.frame, say A and B. The output is A\\B and A 
intersection B.
}
\usage{
available.desired.dataFrames(dataFrame1,dataFrame2,specialCharacter="\r")
}

\arguments{
  \item{dataFrame1}{a data.frame}
  \item{dataFrame2}{a data.frame}
  \item{specialCharacter}{the character used to split the string used to join all data}
}
\details{
 This function must be reworked using the merge function.
}
\value{
A list containing two data.frame: dataFrame1=A\\B.and dataFrame2=A inters B.
}
\author{Claudio Ortelli}
\keyword{ utilities}
