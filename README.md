# 🏦 Customer Churn Prediction for a U.S. Bank

This project applies machine learning techniques to predict customer churn based on credit card usage data from a U.S. bank. The goal is to identify customers at risk of leaving, enabling proactive retention strategies.

## 📌 Project Overview

Customer churn is a critical issue in the financial sector. Understanding the drivers behind customer cancellation can help banks implement targeted interventions and improve customer retention.

This project involves:
- Analyzing customer behavior
- Engineering predictive features
- Building classification models to predict churn
- Interpreting model outputs to drive business decisions

## 📊 Dataset

The dataset contains anonymized customer-level details including:
- Credit card usage patterns
- Transaction history
- Tenure and account features
- Customer service interactions
- Churn labels (binary: 1 = churned, 0 = retained)

*(Note: Dataset source is not publicly available due to confidentiality. A synthetic version can be uploaded for demonstration.)*

## 🔧 Technologies Used

- **Python 3.9**
- **scikit-learn**
- **Pandas, NumPy**
- **Matplotlib, Seaborn** (for visualization)
- **Jupyter Notebook**
- **Git/GitHub** for version control

## 🧠 ML Models Implemented

- Logistic Regression
- Random Forest Classifier
- XGBoost
- Model evaluation using:
  - ROC-AUC Score
  - Accuracy, Precision, Recall, F1
  - Confusion Matrix
  - Cross-validation
  - GridSearchCV for hyperparameter tuning

## ⚙️ How to Run the Project

1. Clone this repository:
   ```bash
   git clone https://github.com/your-username/churn-prediction-bank.git
   cd churn-prediction-bank
