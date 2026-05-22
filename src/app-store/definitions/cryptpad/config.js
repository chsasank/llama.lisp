module.exports = {
    httpUnsafeOrigin: process.env.CPAD_MAIN_DOMAIN || 'http://localhost',
    httpSafeOrigin: process.env.CPAD_SANDBOX_DOMAIN || 'http://localhost',
    httpAddress: '0.0.0.0',
    adminEmail: process.env.CPAD_ADMIN_EMAIL || 'admin@example.com',
    removeDonateButton: true,
    office: {
        url: process.env.CPAD_OFFICE_URL || '',
        secret: process.env.CPAD_OFFICE_SECRET || '',
    },
};
