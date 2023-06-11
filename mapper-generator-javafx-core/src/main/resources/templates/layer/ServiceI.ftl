package ${PACKAGE};

import vip.tuoyang.base.core.bean.response.Page;
import <#if package_prefix??>${package_prefix}.</#if>bean.dto<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}DTO;
import <#if package_prefix??>${package_prefix}.</#if>bean.dto<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}PageDTO;
import <#if package_prefix??>${package_prefix}.</#if>bean.request<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}Cmd;
import <#if package_prefix??>${package_prefix}.</#if>bean.request<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}PageQry;

/**
 * @author ${author}
 * @date ${CUR_DATE_TIME}
 */
public interface ${TYPE_NAME_UPPER_CAMEL}ServiceI {
    /**
     * 添加${DOMAIN_DESC}
     *
     * @param cmd {@link ${TYPE_NAME_UPPER_CAMEL}Cmd}
     */
    void add${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}Cmd cmd);

    /**
     * 分页获取${DOMAIN_DESC}列表
     *
     * @param qry {@link ${TYPE_NAME_UPPER_CAMEL}PageQry}
     * @return {@link ${TYPE_NAME_UPPER_CAMEL}PageDTO}s
     */
    Page<${TYPE_NAME_UPPER_CAMEL}PageDTO> get${TYPE_NAME_UPPER_CAMEL}Page(${TYPE_NAME_UPPER_CAMEL}PageQry qry);

    /**
     * 根据 id 获取${DOMAIN_DESC}
     *
     * @param id id
     * @return {@link ${TYPE_NAME_UPPER_CAMEL}DTO}
     */
    ${TYPE_NAME_UPPER_CAMEL}DTO get${TYPE_NAME_UPPER_CAMEL}ById(Long id);

    /**
     * 修改${DOMAIN_DESC}
     *
     * @param cmd {@link ${TYPE_NAME_UPPER_CAMEL}Cmd}
     */
    void update${TYPE_NAME_UPPER_CAMEL}ById(${TYPE_NAME_UPPER_CAMEL}Cmd cmd);

    /**
     * 根据 id 删除${DOMAIN_DESC}
     *
     * @param id id
     */
    void delete${TYPE_NAME_UPPER_CAMEL}ById(Long id);
}