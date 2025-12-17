<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import com.sy.common.bo.Page;
<#assign suffixs = ["DTO", "PageDTO", "PageVO", "VO"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
public interface ${TYPE_NAME_UPPER_CAMEL}Service {
    /**
     * 添加 ${DOMAIN_DESC}
     *
     * @param dto {@link ${TYPE_NAME_UPPER_CAMEL}DTO}
     */
    void add${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}DTO dto);

    /**
     * 分页获取 ${DOMAIN_DESC} 列表
     *
     * @param dto {@link ${TYPE_NAME_UPPER_CAMEL}PageDTO}
     * @return {@link ${TYPE_NAME_UPPER_CAMEL}PageVO}s
     */
    Page<${TYPE_NAME_UPPER_CAMEL}PageVO> list${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}PageDTO dto);

    /**
     * 根据 id 获取 ${DOMAIN_DESC}
     *
     * @param id id
     * @return {@link ${TYPE_NAME_UPPER_CAMEL}VO}
     */
    ${TYPE_NAME_UPPER_CAMEL}VO get${TYPE_NAME_UPPER_CAMEL}ById(Long id);

    /**
     * 修改 ${DOMAIN_DESC}
     *
     * @param dto {@link ${TYPE_NAME_UPPER_CAMEL}DTO}
     */
    void update${TYPE_NAME_UPPER_CAMEL}ById(${TYPE_NAME_UPPER_CAMEL}DTO dto);

    /**
     * 根据 id 删除 ${DOMAIN_DESC}
     *
     * @param id id
     */
    void delete${TYPE_NAME_UPPER_CAMEL}ById(Long id);
}