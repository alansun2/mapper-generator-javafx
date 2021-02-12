package org.mybatis.generator.my.config;


/**
 * @author AlanSun
 * @date 2020/9/10 14:14
 */
public class ServiceConfig {
    private String configName;
    private String requestPackage;
    /**
     * request 全局忽略字段
     */
    private String requestGlobalIgnoreColumns;
    private String servicePackage;
    private String controllerPackage;

    /**
     * 是否生成 validation 注解，例如 @Length
     */
    private boolean isGenerateValidationAnnotation = true;

    public boolean isGenerateValidationAnnotation() {
        return isGenerateValidationAnnotation;
    }

    public void setGenerateValidationAnnotation(boolean generateValidationAnnotation) {
        isGenerateValidationAnnotation = generateValidationAnnotation;
    }

    public String getConfigName() {
        return configName;
    }

    public void setConfigName(String configName) {
        this.configName = configName;
    }

    public String getRequestPackage() {
        return requestPackage;
    }

    public void setRequestPackage(String requestPackage) {
        this.requestPackage = requestPackage;
    }

    public String getServicePackage() {
        return servicePackage;
    }

    public void setServicePackage(String servicePackage) {
        this.servicePackage = servicePackage;
    }

    public String getControllerPackage() {
        return controllerPackage;
    }

    public void setControllerPackage(String controllerPackage) {
        this.controllerPackage = controllerPackage;
    }

    public String getRequestGlobalIgnoreColumns() {
        return requestGlobalIgnoreColumns;
    }

    public void setRequestGlobalIgnoreColumns(String requestGlobalIgnoreColumns) {
        this.requestGlobalIgnoreColumns = requestGlobalIgnoreColumns;
    }
}
