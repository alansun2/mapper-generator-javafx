<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
<#assign suffixs = ["DO", ""]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

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
