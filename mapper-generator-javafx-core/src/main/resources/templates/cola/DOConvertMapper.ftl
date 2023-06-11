package ${PACKAGE};

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
import <#if package_prefix??>${package_prefix}.</#if>domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}DO;
import <#if package_prefix??>${package_prefix}</#if><#if DOMAIN != "">.${DOMAIN}</#if>.dataobject.${TYPE_NAME_UPPER_CAMEL};

/**
 * @author ${author}
 * @date ${CUR_DATE_TIME}
 * <p>
 * DO 转换
 */
@Mapper
public interface ${TYPE_NAME_UPPER_CAMEL}DOConvertMapper {

    ${TYPE_NAME_UPPER_CAMEL}DOConvertMapper INSTANCE = Mappers.getMapper(${TYPE_NAME_UPPER_CAMEL}DOConvertMapper.class);

    ${TYPE_NAME_UPPER_CAMEL}DO convertToDO(${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL});
}
