package org.mybatis.generator.my.config;

/**
 * @author AlanSun
 * @date 2020/12/3 11:20
 */
public class ServiceConfigThreadLocal {

    private static ThreadLocal<ServiceConfig> serviceConfigThreadLocal = new ThreadLocal<>();

    public static ServiceConfig getServiceConfig() {
        return serviceConfigThreadLocal.get();
    }

    public static void setServiceConfig(ServiceConfig serviceConfig) {
        serviceConfigThreadLocal.set(serviceConfig);
    }

    public static void remove() {
        serviceConfigThreadLocal.remove();
    }
}
