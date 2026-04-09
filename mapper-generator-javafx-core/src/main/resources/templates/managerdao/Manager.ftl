<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

<#assign suffixs = ["DTO", "", "PageVO", "VO"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>
import org.springframework.stereotype.Component;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
@Component
public class ${TYPE_NAME_UPPER_CAMEL}Manager {

    public ${TYPE_NAME_UPPER_CAMEL} convert(final ${TYPE_NAME_UPPER_CAMEL}DTO dto) {
        <@ic.ignoreColumnCheck "${TYPE_NAME_UPPER_CAMEL}" "${TYPE_NAME_LOWER_CAMEL}" "dto" "null" "DTO" FIELDS_UPPER_CAMELS IGNORE_FIELDS_MAP true !/>
        return ${TYPE_NAME_LOWER_CAMEL};
    }

    public ${TYPE_NAME_UPPER_CAMEL}PageVO convert2PageVO(final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        return this.convert2VO(${TYPE_NAME_LOWER_CAMEL}, new ${TYPE_NAME_UPPER_CAMEL}PageVO());
    }

    public ${TYPE_NAME_UPPER_CAMEL}VO convert2VO(final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        return this.convert2VO(${TYPE_NAME_LOWER_CAMEL}, new ${TYPE_NAME_UPPER_CAMEL}VO());
    }

    private <T extends ${TYPE_NAME_UPPER_CAMEL}VO> T convert2VO(final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}, T ${TYPE_NAME_LOWER_CAMEL}VO) {
        <@ic.ignoreColumnCheck "${TYPE_NAME_UPPER_CAMEL}VO" "${TYPE_NAME_LOWER_CAMEL}VO" "${TYPE_NAME_LOWER_CAMEL}" "VO" "DO" FIELDS_UPPER_CAMELS IGNORE_FIELDS_MAP false !/>
        return ${TYPE_NAME_LOWER_CAMEL}VO;
    }
}