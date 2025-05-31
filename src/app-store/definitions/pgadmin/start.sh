cat > /run/pgadmin/servers.json << END
{
    "Servers": {
        "1": {
            "Name": "PSQL",
            "Group": "Servers",
            "Port": 5432,
            "Username": "pgadmin",
            "Host": "localhost",
            "Password": "$POSTGRES_PASSWORD",
            "SSLMode": "prefer",
            "MaintenanceDB": "postgres"
        }
    }
}
END
/entrypoint.sh
