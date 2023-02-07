package ${PACKAGE};

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
import ${package_prefix}.domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}DO;
import ${package_prefix}<#if DOMAIN != "">.${DOMAIN}</#if>.dataobject.${TYPE_NAME_UPPER_CAMEL};

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 * <p>
 * DO 转换
 */
@Mapper
public interface ${TYPE_NAME_UPPER_CAMEL}DOConvertMapper {

    ${TYPE_NAME_UPPER_CAMEL}DOConvertMapper INSTANCE = Mappers.getMapper(${TYPE_NAME_UPPER_CAMEL}DOConvertMapper.class);

    ${TYPE_NAME_UPPER_CAMEL}DO convertToDO(${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL});
}
