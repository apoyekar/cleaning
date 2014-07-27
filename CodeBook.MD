# Feature Selection 

The features selected for this database come from the raw dataset received from [https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

Test and training data is combined and only columns that contains mean() and std() of different measurements are selected.

Finally averages of these columns by each subject and each activity are derived.

# DATA DICTIONARY

1. "subject" 

Represents observer id (1-30)

Data Type: integer

1. "activity" 


Represents activity label from the following

WALKING

WALKING\_UPSTAIRS

WALKING\_DOWNSTAIRS

SITTING

STANDING

LAYING

Data Type: factor



Features from 3 to 68 represents average value of different measurements of mean and standard deviations by each subject and each activity. If variable name contains Accelerometer, data comes from Accelerometer, and if it contains gyroscope it comes from Gyroscope. X, Y and Z are used to denote 3-axial signals in the X, Y and Z directions. f denotes Fast Fourier Transform (FFT) was applied to the original data and t denotes time domain signals. Acceleration signal are separated into body and gravity acceleration signals which are represented by variables that contain Body and Gravity in it.

Data Types for 3 to 68 : numeric

1. "tBodyAccelerometerMeanXAvg" 
2. "tBodyAccelerometerMeanYAvg" 
3. "tBodyAccelerometerMeanZAvg" 
4. "tBodyAccelerometerStdXAvg" 
5. "tBodyAccelerometerStdYAvg" 
6. "tBodyAccelerometerStdZAvg" 
7. "tGravityAccelerometerMeanXAvg" 
8. "tGravityAccelerometerMeanYAvg" 
9. "tGravityAccelerometerMeanZAvg" 
10. "tGravityAccelerometerStdXAvg" 
11. "tGravityAccelerometerStdYAvg" 
12. "tGravityAccelerometerStdZAvg" 
13. "tBodyAccelerometerJerkMeanXAvg" 
14. "tBodyAccelerometerJerkMeanYAvg" 
15. "tBodyAccelerometerJerkMeanZAvg" 
16. "tBodyAccelerometerJerkStdXAvg" 
17. "tBodyAccelerometerJerkStdYAvg" 
18. "tBodyAccelerometerJerkStdZAvg" 
19. "tBodyGyroscoperMeanXAvg" 
20. "tBodyGyroscoperMeanYAvg" 
21. "tBodyGyroscoperMeanZAvg" 
22. "tBodyGyroscoperStdXAvg" 
23. "tBodyGyroscoperStdYAvg" 
24. "tBodyGyroscoperStdZAvg" 
25. "tBodyGyroscoperJerkMeanXAvg" 
26. "tBodyGyroscoperJerkMeanYAvg" 
27. "tBodyGyroscoperJerkMeanZAvg" 
28. "tBodyGyroscoperJerkStdXAvg" 
29. "tBodyGyroscoperJerkStdYAvg" 
30. "tBodyGyroscoperJerkStdZAvg" 
31. "tBodyAccelerometerMagMeanAvg" 
32. "tBodyAccelerometerMagStdAvg" 
33. "tGravityAccelerometerMagMeanAvg" 
34. "tGravityAccelerometerMagStdAvg" 
35. "tBodyAccelerometerJerkMagMeanAvg" 
36. "tBodyAccelerometerJerkMagStdAvg" 
37. "tBodyGyroscoperMagMeanAvg" 
38. "tBodyGyroscoperMagStdAvg" 
39. "tBodyGyroscoperJerkMagMeanAvg" 
40. "tBodyGyroscoperJerkMagStdAvg" 
41. "fBodyAccelerometerMeanXAvg" 
42. "fBodyAccelerometerMeanYAvg" 
43. "fBodyAccelerometerMeanZAvg" 
44. "fBodyAccelerometerStdXAvg" 
45. "fBodyAccelerometerStdYAvg" 
46. "fBodyAccelerometerStdZAvg" 
47. "fBodyAccelerometerJerkMeanXAvg" 
48. "fBodyAccelerometerJerkMeanYAvg" 
49. "fBodyAccelerometerJerkMeanZAvg" 
50. "fBodyAccelerometerJerkStdXAvg" 
51. "fBodyAccelerometerJerkStdYAvg" 
52. "fBodyAccelerometerJerkStdZAvg" 
53. "fBodyGyroscoperMeanXAvg" 
54. "fBodyGyroscoperMeanYAvg" 
55. "fBodyGyroscoperMeanZAvg" 
56. "fBodyGyroscoperStdXAvg" 
57. "fBodyGyroscoperStdYAvg" 
58. "fBodyGyroscoperStdZAvg" 
59. "fBodyAccelerometerMagMeanAvg" 
60. "fBodyAccelerometerMagStdAvg" 
61. "fBodyBodyAccelerometerJerkMagMeanAvg" 
62. "fBodyBodyAccelerometerJerkMagStdAvg" 
63. "fBodyBodyGyroscoperMagMeanAvg" 
64. "fBodyBodyGyroscoperMagStdAvg" 
65. "fBodyBodyGyroscoperJerkMagMeanAvg" 
66. "fBodyBodyGyroscoperJerkMagStdAvg"
