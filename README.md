1. Dataset Title

Dataset on student engagement in teaching evaluations: Organizational identification, institutional trust, perceived usefulness, and constructive feedback behavior.

2. Dataset Description

This dataset contains survey responses examining students’ perceptions and behaviors related to Student Evaluation of Teaching (SET) in a higher education context.

The dataset was collected to investigate the institutional and psychological factors associated with students’ willingness to provide constructive feedback in course evaluations.

The conceptual model includes the following constructs:

- Organizational Identification

- Institutional Trust

- Perceived Usefulness of Student Evaluations

- Constructive Evaluation Behavior

- Self-Perceived Evaluation Bias

The dataset supports research examining student participation in teaching evaluation systems and can be used for statistical analysis, structural equation modeling (SEM), or replication studies.

3. Data Collection

- Population: Undergraduate students

- Institution: A private university in Vietnam

- Fields of study: Software Engineering and Business

- Data collection method: Online questionnaire (Google Forms)

- Collection period: Late February – early March 2026

- Sample size: 362 respondents

- Response format: Five-point Likert scale

Participation in the survey was voluntary and anonymous, and no personally identifiable information was collected.

4. Dataset Structure

The dataset file: dataset_SET_student_evaluation_raw.xlsx

Each row represents one respondent.

Dataset Variables

Variable            Description
ID                  Anonymous respondent identifier
gender              Respondent gender
year_of_study       Academic year (1–4)
program             Field of study
OI1–OI5             Organizational Identification items
TR1–TR5             Institutional Trust items
PU1–PU5             Perceived Usefulness of SET items
CB1–CB5             Constructive Evaluation Behavior items
PB1–PB5             Self-Perceived Evaluation Bias items
PC1–PC5             Process Clarity items

All survey items were measured using a five-point Likert scale:

1 = Strongly disagree;
2 = Disagree;
3 = Neutral;
4 = Agree;
5 = Strongly agree


5. Survey Instrument

The original questionnaire contained 30 survey items across six conceptual domains.

The complete instrument is provided in the file: questionnaire_30items.pdf

6. Analysis Code

The file: R_analysis_script.R

contains the R code used for data preparation and statistical analysis, including:

- Data import and preprocessing

- Descriptive statistics

- Confirmatory factor analysis (CFA)

- Structural equation modeling (SEM)

- Robustness checks

The analysis was conducted using the R statistical environment, primarily with the lavaan package.

The dataset contains 30 survey items across six conceptual domains. For the associated SEM analysis, a reduced 15-item measurement model
was retained after reliability and validity assessment.

7. Ethical Considerations

Participation in the survey was voluntary and based on informed consent.
All responses were collected anonymously, and no personally identifiable information was recorded.

The dataset shared in this repository has been fully anonymized.

8. Data Reuse

The dataset is provided to support research on:

- Student engagement in higher education

- Student evaluation of teaching (SET)

- Institutional trust and organizational identification

- Educational survey data analysis

- Structural equation modeling applications in education research

Researchers are encouraged to reuse the dataset for replication studies, methodological research, or comparative studies across higher education systems.

9. Related Research Article

This dataset accompanies the research article: Student Engagement in Teaching Evaluations: How Organizational Identification and Institutional Trust Shape Constructive Feedback

10. Contact

For questions regarding the dataset or research, please contact:

Tran Trong Huynh
Department of Mathematics, FPT University, Ho Chi Minh City, Vietnam 
E-mail: huynhtt4@fe.edu.vn
