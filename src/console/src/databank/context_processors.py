from django.conf import settings


def hyperdx(request):
    return {
        "HYPERDX_OTEL_URL": settings.HYPERDX_OTEL_URL,
        "HYPERDX_BROWSER_API_KEY": settings.HYPERDX_BROWSER_API_KEY,
        "ETL_API_HOST": settings.ETL_API_HOST,
    }
