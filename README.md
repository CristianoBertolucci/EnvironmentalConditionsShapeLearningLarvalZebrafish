# EnvironmentalConditionsShapeLearningLarvalZebrafish
The present study aimed to investigate whether early experiences of environmental complexity might shape òearning performance in zebrafish (_Danio rerio_) larvae. Briefly, newly-hatched larvae (3-days post fertilization) were exposed to either an enriched environment containing Lego® bricks or barren conditions. Then, a control test based on the response to a novel environment (Open-Field Test) and a Vibrational Startle Response Assay were performed to assess individual differences due to the environmental conditions experienced.

This is the main repository containing the .xlsx data files and the R source code used for assessing differences due to the treatment condition.


# Get Started
## .xlsx file "Dataset"  
Three datasets were included in the main file:

- Sheet 2 = behavioral parameters (i.e., distance moved across the apparatus and the number of inspections towards the centre) defining response to a novel environment. Parameters were repeated measures within-subject (six 5-minute block)

- Sheet 3 = habituation learning index computed by considering the distance moved in response to a repeated stimulation as following:

  **(distance moved in the x<sub>-th stimulation</sub> - distance moved x<sub>first stimulation</sub>) / distance moved x<sub>first stimulation</sub>)**.

  Datapoints were represented as average index response ("mean_index_log") and as repeated measures within ("index_log").

- Sheet 4 = average total length of larvae


## Rsource code "Rcode_analysis"  
The source code to execute the analysis and to generate the graph.
"R version 4.2.1 (2022-06-23 ucrt)"
platform       x86_64-w64-mingw32  



# General Discussion and Q&A
For any further request, anyone is welcome to participate on the discussion in the forum or directly contact the authors

Elia Gatto<sup>1,2</sup>  
Tyone Lucon-Xiccato<sup>2</sup>  
Cristiano Bertolucci<sup>2</sup>  
1. Department of Chemical, Pharmaceutical and Agricultural Sciences, University of Ferrara, Ferrara, Italy
2. Department of Life Sciences and Biotechnology, University of Ferrara, Ferrara, Italy
