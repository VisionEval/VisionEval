% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AdjustVehicleOwnership.R
\name{AdjustVehicleOwnership}
\alias{AdjustVehicleOwnership}
\title{Adjust household vehicle ownership when car service cost is less.}
\usage{
AdjustVehicleOwnership(L)
}
\arguments{
\item{L}{A list containing the components listed in the Get specifications
for the module.}
}
\value{
A list containing the components specified in the Set
specifications for the module.
}
\description{
\code{AdjustVehicleOwnership} adjusts household vehicle ownership by
substituting use of car service when the level of car service is high and
when cost per mile to use car service is less than the cost per mile to
own vehicle
}
\details{
This function calculates the ownership cost per mile for household vehicles
and compares with the cost per mile to use car service vehicles if the level
of car service is high. If ownership is more costly for a vehicle,
substitution is determined by random draw using the car service substitution
probability for the vehicle type. If a substitution is made, the vehicle
access status is changed from 'Own' to 'HighCarSvc'. The ownership cost is
changed to 0 as is the insurance cost. The household vehicle inventory is
also updated.
}
