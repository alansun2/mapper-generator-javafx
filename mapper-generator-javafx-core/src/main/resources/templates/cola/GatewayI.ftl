package ${PACKAGE};

import java.util.List;
import java.util.Optional;

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 */
public interface ${TYPE_NAME_UPPER_CAMEL}GatewayI {
    /**
     * 添加${DOMAIN_DESC}
     *
     * @param ${TYPE_NAME_LOWER_CAMEL}DO {@link ${TYPE_NAME_UPPER_CAMEL}DO}
     */
    void save${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO);

    /**
    * 批量添加${DOMAIN_DESC}
    *
    * @param ${TYPE_NAME_LOWER_CAMEL}DOS {@link ${TYPE_NAME_UPPER_CAMEL}DO}s
    */
    void save${TYPE_NAME_UPPER_CAMEL}Batch(List<${TYPE_NAME_UPPER_CAMEL}DO> ${TYPE_NAME_LOWER_CAMEL}DOS);

    /**
     * 更新${DOMAIN_DESC}
     *
     * @param ${TYPE_NAME_LOWER_CAMEL}DO {@link ${TYPE_NAME_UPPER_CAMEL}DO}
     */
    void update${TYPE_NAME_UPPER_CAMEL}ById(${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO);

    /**
     * 根据id获取${DOMAIN_DESC}
     *
     * @param id id
     * @return {@link ${TYPE_NAME_UPPER_CAMEL}DO}
     */
    Optional<${TYPE_NAME_UPPER_CAMEL}DO> get${TYPE_NAME_UPPER_CAMEL}ById(Long id);

    /**
    * 根据 id 列表获取${DOMAIN_DESC}列表
    *
    * @param ids ids
    * @param isNeedDelete 是否包含已删除
    * @return {@link ${TYPE_NAME_UPPER_CAMEL}DO}s
    */
    List<${TYPE_NAME_UPPER_CAMEL}DO> list${TYPE_NAME_UPPER_CAMEL}ByIds(List<Long> ids, boolean isNeedDelete);
}