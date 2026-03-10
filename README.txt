Data and Analysis Code for Early Life Stress Experiments
Experiments conducted 2024 and 2025

Code: Combined ELS Life History Analysis.R
Data: Combined ELS Data.csv, 2025 ELS Data.csv, 2024 ELS Data.csv
File Metadata: 
	Year: Experimental Cohort
	Block: Related to grouping in field, see map for blocks	
	RGP: plant id, with row number, group number (plants are planted in groups of 4), and plant number (4 plants per plot)
	Treatment: Full treatment name, see manuscript for details
	RosDam: Whether or not the assigned treatment included rosette damage (y/n)
	EarlyMow: Whether or not the assigned treatment included an early mow (y/n)
	LateMow: Whether or not the assigned treatment included a late mow (y/n)
	FinCap: Total number of capitula on the plant at experiment termination (including those not mature yet)
	FinMatureCap: Total number of mature capitula on the plant at experiment termination
	FloweringDate_doe: Day of experiment (after early life stress event) on which flowering first occurred, defined as the presence of at least one anthesed capitulum on a plant that had received all of its assigned damages
	FloweringDate_doy: Day of year (from January 1st) on which flowering first occurred, defined as the presence of at least one anthesed capitulum on a plant that had received all of its assigned damages
	HeightatFlowering: Height of plant in cm when flowering occurred, defined as the presence of at least one anthesed capitulum on a plant that had received all of its assigned damages
	MaxCapitula: the maximum number of capitula, including immature capitula, that were ever present on the plant, determined for each plant after all assigned physical damage had been completed
	MaxCapitulaIncPreTrtSeedProducers: MaxCapitula, plus the number of capitula that produced seeds on each plant before all physical damage was completed (very few capitula matured pre-treatment)
	MaxHeight: the maximum height in cm that each plant reached after all physical damage treatments had been applied
	LLLSpring: longest leaf length in cm on May 2, 2024 or April 30, 2025 (start of experiment)
		In 2024 and 2025 data, this variable is called LLLMay2 and LLLApr30, respectively
	NoStemsFinal: Number of upright stems on the plant at experiment termination
	NoStemsSpring: Number of stems on May 2, 2024 or April 30, 2025 (start of experiment)
  
Data: Combined Mature Capitulum Size Data.csv, 2025 Mature Capitulum Size Data.csv, 2024 Mature Capitulum Size Data.csv
File Metadata:
	Year: Experimental cohort
	Block: Related to grouping in field, see map for blocks
	RGP: plant id, with row number, group number (plants are planted in groups of 4), and plant number (4 plants per plot)
	Treatment: Full treatment name, see manuscript for details
	Capitulum: Capitulum number within the same plant
	AvgCapDia: Diameter (cm) of each capitulum. This is the average of the two values taken for each capitulum.

Code: Combined Survival Analysis.R
Data: SurvDatCombined.csv, SurvData2025.csv, SurvData2024.csv
File Metadata:
	Year: Experimental cohort
	Block: Related to grouping in field, see map for blocks
	RGP: plant id, with row number, group number (plants are planted in groups of 4), and plant number (4 plants per plot)
	Treatment: Full treatment name, see manuscript for details
	TimeOfDeath_doe: day of the experiment (after early life stress) on which plant was recorded as dead
	TimeOfDeath_doy: day of the year (since January 1st) on which a plant was recorded as dead
	Status: death (1) or censoring (0) on the date indicated

Code: Post Mowing LLL Analysis
Data: Combined Post Mowing LLL Data.csv, 2025 Post Mowing LLL Data.csv, 2024 Post Mowing LLL Data.csv
File Metadata:
	Row: Row number in the field
	Group: Group number (plants are planted in groups of 4)
	Plant: Plant number (4 plants per plot)
	Treatment: Full treatment name, see manuscript for details
	PostMowLLL: Longest leaf length (cm) in the week immediately following the a plant's first mow

Code: Pre Mowing LLL Analysis
Data: Combined Pre Mowing LLL Data.csv, 2025 Pre Mowing LLL Data.csv, 2024 Pre Mowing LLL Data.csv
File Metadata:
	Row: Row number in the field
	Group: Group number (plants are planted in groups of 4)
	Plant: Plant number (4 plants per plot)
	Treatment: Full treatment name, see manuscript for details
	PostMowLLL: Longest leaf length (cm) in the week immediately following the a plant's first mow