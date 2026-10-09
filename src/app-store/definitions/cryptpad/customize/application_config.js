(() => {
const factory = (AppConfig) => {

    AppConfig.loginSalt = '874e24ac966d577a2442b513fb465427af09a761d3a10976516256fc900c7519';
    AppConfig.minimumPasswordLength = 8;

    // Force login - block all anonymous access
    AppConfig.registeredOnlyTypes = AppConfig.availablePadTypes;
    AppConfig.disableAnonymousPadCreation = true;
    AppConfig.disableAnonymousStore = true;

    return AppConfig;
};

if (typeof(module) !== 'undefined' && module.exports) {
    module.exports = factory(
        require('../www/common/application_config_internal.js')
    );
} else if ((typeof(define) !== 'undefined' && define !== null) && (define.amd !== null)) {
    define(['/common/application_config_internal.js'], factory);
}
})();
