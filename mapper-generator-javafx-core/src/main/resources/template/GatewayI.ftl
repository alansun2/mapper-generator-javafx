package ${PACKAGE};

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 */
public interface ${TYPE_NAME_UPPER_CAMEL}GatewayI {
    /**
     * 新增${DOMAIN_DESC}
     *
     * @param ${TYPE_NAME_LOWER_CAMEL}DO {@link ${TYPE_NAME_UPPER_CAMEL}DO}
     */
    void save${DOMAIN_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO);

    /**
     * 更新${DOMAIN_DESC}
     *
     * @param ${TYPE_NAME_LOWER_CAMEL}DO {@link ${TYPE_NAME_UPPER_CAMEL}DO}
     */
    void update${DOMAIN_UPPER_CAMEL}ById(${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO);

    /**
     * 根据id获取${DOMAIN_DESC}
     *
     * @param id id
     * @return ${TYPE_NAME_LOWER_CAMEL}DO {@link ${TYPE_NAME_UPPER_CAMEL}DO}
     */
    ${TYPE_NAME_UPPER_CAMEL}DO get${DOMAIN_UPPER_CAMEL}ById(Long id);
}