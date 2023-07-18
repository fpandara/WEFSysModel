#Function to simulate WEF nexus for the WEFSys app
#The input data comes from data entered by the user in the online interface

WEFmodel <- function(Y,parms,InputData) {
  ##Extract parameters##
  TotalArea <- parms['TotalArea']
  Crop1EnergyDemand <- parms['Crop1EnergyDemand'] # in kWh per metric tons
  Crop2EnergyDemand <- parms['Crop2EnergyDemand']
  Crop3EnergyDemand <- parms['Crop3EnergyDemand']
  LSEnergyDemand <- parms['LSEnergyDemand'] # in kWh
  DomEnergyDemand <- parms['DomEnergyDemand']
  IndEnergyDemand <- parms['IndEnergyDemand']
  Crop1WaterDemand <- parms['Crop1WaterDemand'] # in m^3 per metric tons
  Crop2WaterDemand <- parms['Crop2WaterDemand']
  Crop3WaterDemand <- parms['Crop3WaterDemand']
  LSWaterDemand <- parms['LSWaterDemand'] # in m^3
  DomWaterDemand <- parms['DomWaterDemand']
  IndWaterDemand <- parms['IndWaterDemand']
  DomFoodDemand <- parms['DomFoodDemand']
  
  WEIntensity <- parms['WEIntensity']
  EWIntensity <- parms['EWIntensity']
  
  Crop1coeff <- parms['Crop1coeff'] # in kcal per grams
  Crop2coeff <- parms['Crop2coeff']
  Crop3coeff <- parms['Crop3coeff']
  
  
  DrinkingWaterAccess <- parms['DrinkingWaterAccess']
  ElectricityAccess <- parms['ElectricityAccess']
  FoodAccess <- parms['FoodAccess']
  HHFuelAccess <- parms['HHFuelAccess']
  
  BirthRate <- parms['BirthRate']
  DeathRate <- parms['DeathRate']
  ImmigrationRate <- parms['ImmigrationRate']
  OutmigrationRate <- parms['OutmigrationRate']
  Crop1rate <- parms['Crop1rate']
  Crop2rate <- parms['Crop2rate']
  Crop3rate <- parms['Crop3rate']
  Livestockrate <- parms['Livestockrate']
  Residentialrate <- parms['Residentialrate']
  Industrialrate <- parms['Industrialrate']
  
  YearStart <- parms['YearStart']
  YearEnd <-parms['YearEnd']
  NumYears <- YearEnd - YearStart + 1
  
  
  # Initialize output vectors 
  Population <- numeric(NumYears)
  Crop1Area <- numeric(NumYears)  
  Crop2Area <- numeric(NumYears)  
  Crop3Area <- numeric(NumYears)
  LivestockArea <- numeric(NumYears)  
  ResidentialArea <- numeric(NumYears)  
  IndustrialArea <- numeric(NumYears)
  Total_water_demand <- numeric(NumYears)
  Total_water_supply <- numeric(NumYears)
  Total_energy_demand <- numeric(NumYears)
  Total_energy_supply <- numeric(NumYears)
  Total_food_demand <- numeric(NumYears)
  Total_food_supply <- numeric(NumYears)
  Water_security <- numeric(NumYears) 
  Energy_security <- numeric(NumYears)  
  Food_security <- numeric(NumYears)
  WEF_security <- numeric(NumYears)
  APP_water <- numeric(NumYears)
  APP_energy <- numeric(NumYears)
  APP_food <- numeric(NumYears)
  SSL_water <- numeric(NumYears)
  SSL_energy <- numeric(NumYears)
  SSL_food <- numeric(NumYears)
  

  Population[1] <- Y['Population']   
  Crop1Area[1] <- Y['Crop1Area']  
  Crop2Area[1] <- Y['Crop2Area']  
  Crop3Area[1] <- Y['Crop3Area']   
  LivestockArea[1] <- Y['LivestockArea']   
  ResidentialArea[1] <- Y['ResidentialArea']   
  IndustrialArea[1] <- Y['IndustrialArea']   
  Total_water_demand[1] <- Y['Total_water_demand']
  Total_water_supply[1] <- Y['Total_water_supply']
  Total_energy_demand[1] <- Y['Total_energy_demand']
  Total_energy_supply[1] <- Y['Total_energy_supply']
  Total_food_demand[1] <- Y['Total_food_demand']
  Total_food_supply[1] <- Y['Total_food_supply']
  Energy_security[1] <- Y['Energy_security']
  Water_security[1] <- Y['Water_security']
  Food_security[1] <- Y['Food_security']
  WEF_security[1] <- Y['WEF_security']
  APP_water[1] <- Y['App_water']
  APP_energy[1] <- Y['App_energy']
  APP_food[1] <- Y['App_food']
  SSL_water[1] <- Y['SSL_water']
  SSL_energy[1] <- Y['SSL_energy']
  SSL_food[1] <- Y['SSL_food']

  ##Loop through the years to get variable values for each year##
	for (t in 1:NumYears) {

	  
	Crop1Yield <- InputData[t,2] # in metric ton/ha
	Crop2Yield <- InputData[t,3]
	Crop3Yield <- InputData[t,4]
	LivestockProductivity <- InputData[t,5] #in kCal/ha
	AnnualRainfall <- InputData[t,6] #in mm

	Crop_1_Production <- Crop1Area[t]*Crop1Yield #production in tons
	Crop_2_Production <- Crop2Area[t]*Crop2Yield
	Crop_3_Production <- Crop3Area[t]*Crop3Yield
	Livestock_Production <- LivestockArea[t]*LivestockProductivity #production in kcal
	Agricultural_area <- LivestockArea[t]+Crop1Area[t]+Crop2Area[t]+Crop3Area[t] # in ha
	
	Outmigration_tot <-  Population[t] * OutmigrationRate
	Deaths_tot <-  DeathRate * Population[t]
	Immigration_tot <-  ImmigrationRate * Population[t]
	Births_tot <-  BirthRate * Population[t]
	

	DomWaterDemand_tot <- DomWaterDemand*Population[t] #in m^3
	DomEnergyDemand_tot <- (DomEnergyDemand*Population[t])/1000 #in MWh
	DomFoodDemand_tot <- DomFoodDemand*Population[t] # in kCal
	Local_food_Production <- (Crop_1_Production*1000000*Crop1coeff)+(Crop_2_Production*1000000*Crop2coeff)+(Crop_3_Production*1000000*Crop3coeff)+Livestock_Production #in kCal
	Crop_water_demand <- (Crop_1_Production*Crop1WaterDemand)+(Crop_2_Production*Crop2WaterDemand)+(Crop_3_Production*Crop3WaterDemand) # in m^3
	C_value <- ((Agricultural_area*0.3)+(IndustrialArea[t]*0.7)+(ResidentialArea[t]*0.5))/TotalArea
	Agricultural_energy_demand <- (((Crop_1_Production*Crop1EnergyDemand)+(Crop_2_Production*Crop2EnergyDemand)+(Crop_3_Production*Crop3EnergyDemand))/1000)+LSEnergyDemand # in MWh
	Agricultural_water_demand <- Crop_water_demand + LSWaterDemand # in m^3
	Natural_water_supply <- 10*C_value*AnnualRainfall*TotalArea # in m^3
	Local_water_Production <- Natural_water_supply
	Food_demand_tot <-  DomFoodDemand_tot # in kCal
	
	
	LocalEnergy = InputData[t,7] # in MWh
	
	if (InputData[t,10]<0) {
	  ImportedFood = 0
	}else {
	  ImportedFood = InputData[t,10]  #in kCal
	}
	if (InputData[t,9]<0) {
	  ImportedEnergy = 0
	}else {
	  ImportedEnergy = InputData[t,9] # in MWh
	}
	if (InputData[t,8]<0) {
	  ImportedWater = 0
	} else {
	  ImportedWater = InputData[t,8] # in m^3
	}
	
  ##Supply and demand variables##
	Food_supply_tot <-  Local_food_Production + ImportedFood # in kCal
	Water_supply_tot <-  Local_water_Production + ImportedWater # in m^3
	Energy_supply_tot <-  ImportedEnergy + LocalEnergy # in MWh
	Energy_for_water <- WEIntensity*Water_supply_tot # in MWh
	Water_for_energy <- Energy_supply_tot*EWIntensity # in m^3
	Water_demand_tot <-  IndWaterDemand + Agricultural_water_demand + Water_for_energy + DomWaterDemand_tot # in m^3
	Energy_demand_tot <-  IndEnergyDemand + DomEnergyDemand_tot + Agricultural_energy_demand + Energy_for_water # in MWh
	
	
	if (InputData[t,10]<0) {
	  ImportedFood = Food_demand_tot - LocalFood #in kCal
	} 
	if (InputData[t,9]<0) {
	  ImportedEnergy = Energy_demand_tot - LocalEnergy #in MWh
	} 
	if (InputData[t,8]<0) {
	  ImportedWater = Water_demand_tot - Local_water_Production #in m^3
	} 

	Food_supply_tot <-  Local_food_Production + ImportedFood #in kCal
	Water_supply_tot <-  Local_water_Production + ImportedWater #in m^3
	Energy_supply_tot <-  ImportedEnergy + LocalEnergy #in MWh
	Energy_for_water <- WEIntensity*Water_supply_tot #in MWh
	Water_for_energy <- Energy_supply_tot*(EWIntensity/1000) #in m^3
	Water_demand_tot <-  IndWaterDemand + Agricultural_water_demand + Water_for_energy + DomWaterDemand_tot #in m^3
	Energy_demand_tot <-  IndEnergyDemand + DomEnergyDemand_tot + Agricultural_energy_demand + Energy_for_water #in MWh
	
  
  ##Estimate APP and SSL indicators##
	ratio_of_water_supply_demand = Water_supply_tot/Water_demand_tot
	ratio_of_energy_supply_demand = Energy_supply_tot/Energy_demand_tot
	ratio_of_food_supply_demand = Food_supply_tot/Food_demand_tot
	APP_food[t] = Food_supply_tot/Population[t]
	APP_water[t] = Water_supply_tot/Population[t]
	APP_energy[t] = Energy_supply_tot/Population[t]
	SSL_water[t] = Local_water_Production/Water_demand_tot
	SSL_food[t] = Local_food_Production/Food_demand_tot
	SSL_energy[t] = LocalEnergy/Energy_demand_tot

	if (ratio_of_water_supply_demand>=1){WI1 = 5} 
	else if (ratio_of_water_supply_demand<1 & ratio_of_water_supply_demand>=0.8)
	{WI1 = 4} else if (ratio_of_water_supply_demand<0.8 & ratio_of_water_supply_demand>=0.6)
	{WI1 = 3} else if (ratio_of_water_supply_demand<0.6 & ratio_of_water_supply_demand>=0.4)
	{WI1 = 2} else
	{WI1 = 1}
	
	if (ratio_of_energy_supply_demand>=1)
	{EI1 = 5} else if (ratio_of_energy_supply_demand<1 & ratio_of_energy_supply_demand>=0.8)
	{EI1 = 4} else if (ratio_of_energy_supply_demand<0.8 & ratio_of_energy_supply_demand>=0.6)
	{EI1 = 3} else if (ratio_of_energy_supply_demand<0.6 & ratio_of_energy_supply_demand>=0.4)
	{EI1 = 2} else
	{EI1 = 1}
	
	if (ratio_of_food_supply_demand>=1)
	{FI1 = 5} else if (ratio_of_food_supply_demand<1 & ratio_of_food_supply_demand>=0.8)
	{FI1 = 4} else if (ratio_of_food_supply_demand<0.8 & ratio_of_food_supply_demand>=0.6)
	{FI1 = 3} else if (ratio_of_food_supply_demand<0.6 & ratio_of_food_supply_demand>=0.4)
	{FI1 = 2} else
	{FI1 = 1}

	if (DrinkingWaterAccess>=0.9)
	{WI2 = 5} else if (DrinkingWaterAccess<0.9 & DrinkingWaterAccess>=0.6)
	{WI2 = 4} else if (DrinkingWaterAccess<0.6 & DrinkingWaterAccess>=0.4)
	{WI2 = 3} else if (DrinkingWaterAccess<0.4 & DrinkingWaterAccess>=0.2)
	{WI2 = 2} else
	{WI2 = 1}

	if (FoodAccess>=0.9)
	{FI3 = 5} else if (FoodAccess<0.9 & FoodAccess>=0.6)
	{FI3 = 4} else if (FoodAccess<0.6 & FoodAccess>=0.4)
	{FI3 = 3} else if (FoodAccess<0.4 & FoodAccess>=0.2)
	{FI3 = 2} else
	{FI3 = 1}

	if (ElectricityAccess>=0.9)
	{EI2 = 5} else if (ElectricityAccess<0.9 & ElectricityAccess>=0.6)
	{EI2 = 4} else if (ElectricityAccess<0.6 & ElectricityAccess>=0.4)
	{EI2 = 3} else if (ElectricityAccess<0.4 & ElectricityAccess>=0.2)
	{EI2 = 2} else
	{EI2 = 1}
	
	if (HHFuelAccess>=0.9)
	{EI3 = 5} else if (HHFuelAccess<0.9 & HHFuelAccess>=0.6)
	{EI3 = 4} else if (HHFuelAccess<0.6 & HHFuelAccess>=0.4)
	{EI3 = 3} else if (HHFuelAccess<0.4 & HHFuelAccess>=0.2)
	{EI3 = 2} else
	{EI3 = 1}

	FI2 = WI2
	FI4 = EI3

	energy_accessibility = EI3*EI2
	energy_availability = EI1
	food_accessibility = FI2*FI3*FI4
  food_availability = FI1
	Water_accessbility = WI2
	Water_availability = WI1

   ##Calculate WEF security indices and WEF supply and demand## 
	 Energy_security[t] = (energy_accessibility*energy_availability)^(1/3)
	 Water_security[t] = (Water_accessbility*Water_availability)^(1/3)
	 Food_security[t] = (food_accessibility*food_availability)^(1/4)
	 WEF_security[t] = (Energy_security[t]*Water_security[t]*Food_security[t])^(1/3)
	 Total_water_demand[t] = Water_demand_tot 
	 Total_water_supply[t] = Water_supply_tot 
	 Total_energy_demand[t] = Energy_demand_tot 
	 Total_energy_supply[t] = Energy_supply_tot 
	 Total_food_demand[t] = Food_demand_tot 
	 Total_food_supply[t] = Food_supply_tot 

	 #Calculate land use areas and population for the next time step##
if (t<NumYears) {
	 Crop1Area[t+1] = Crop1Area[t] *(1+Crop1rate)
	 Crop2Area[t+1] = Crop2Area[t]*(1+Crop2rate)
	 Crop3Area[t+1] = Crop3Area[t] *(1+Crop3rate)
	 LivestockArea[t+1] = LivestockArea[t] *(1+Livestockrate)
	 ResidentialArea[t+1] = ResidentialArea[t]*(1+Residentialrate)
	 IndustrialArea[t+1] = IndustrialArea[t] *(1+Industrialrate)
	 Population[t+1] = Population[t] + Immigration_tot  + Births_tot  - Deaths_tot  - Outmigration_tot
}
	 }
##############################################
##OUTPUT FILE##
##############################################

out <- data.frame(Population,Crop1Area, Crop2Area, Crop3Area, LivestockArea, ResidentialArea, IndustrialArea,
                  Total_water_demand, Total_water_supply, Total_energy_demand,
                    Total_energy_supply, Total_food_demand, Total_food_supply,
                    Water_security, Energy_security, Food_security, WEF_security,
                    APP_water, APP_energy, APP_food, SSL_water, SSL_energy, SSL_food)


}

