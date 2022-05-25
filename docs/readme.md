# Problem statement

Buildings consume approximately one-third of the total energy around the world. It is estimated that more than 20% of this energy is wasted due to equipment failure, aging, misconfiguration, and human-related errors. This wasteful use of energy can be identified and prevented using a data-driven analytical technique known as anomaly or outlier detection. Modern buildings are densely equipped with smart energy meters, which periodically generate a massive amount of time-series data yielding a few million data points every day. This data can be leveraged to identify anomalies present in the energy consumption profiles, which is a big step towards saving energy and achieving global sustainability.

# Objective

In this competition, you will develop accurate models to identify abnormal energy usage instances (point anomaly) in year-long hourly smart electricity meter time series. The provided dataset is the extended version of the dataset used in the ASHRAE - Great Energy Predictor III competition after carefully annotating each data point of electricity meters from approximately 400 commercial buildings.

# Data Description

We provide a large training dataset consisting of year-long hourly electricity meter readings from 200 buildings. Each datapoint in the meter readings is annotated whether it is anomalous (1) or normal (0) usage. Your task is to develop accurate anomaly detection models using this training dataset and then predict whether each each meter reading in the test dataset (from another 206 buildings) is anomalous (1) or normal (0).

# Files
 
train.csv - The training set

```
building_id - Unique building id code.
timestamp - When the measurement was taken
meter_reading - Electricity consumption in kWh.
anomaly - Whether this reading is anomalous (1) or not (0).
```

test.csv - The test set
```
row_id - Unique row id for your submission file.
building_id - Unique building id code.
timestamp - When the measurement was taken
meter_reading - Electricity consumption in kWh.
```

sample_submission.csv - Sample submission file
```
row_id - Unique row id.
anomaly - 1(anomaly) or 0(normal)
```

train_features.csv and test_features.csv - List of features used by the winners of the ASHRAE - Great Energy Predictor III. These are additional features about the buildings and meter readings that might be useful to improve the model prediction.