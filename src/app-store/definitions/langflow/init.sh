set -e
export LANGFLOW_DATABASE_URL="postgresql://langflow:$POSTGRESQL_PASSWORD@localhost:5432/langflow";
langflow run