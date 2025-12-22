<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import com.sy.common.bo.Page;
import com.sy.common.config.response.ResWrapper;
<#assign suffixs = ["DTO", "PageDTO", "Service", "PageVO", "VO"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
@Tag(name = "${DOMAIN_DESC}")
@ResWrapper
@RestController
@RequestMapping(value = "/${TYPE_NAME_LOWER_HYPHEN}")
public class ${TYPE_NAME_UPPER_CAMEL}Controller {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Service ${TYPE_NAME_LOWER_CAMEL}Service;

    /**
     * 添加${DOMAIN_DESC}
     *
     * @param dto {@link ${TYPE_NAME_UPPER_CAMEL}DTO}
     */
    @Operation(summary = "添加${DOMAIN_DESC}")
    @PostMapping
    public void add${TYPE_NAME_UPPER_CAMEL}(@Validated @RequestBody ${TYPE_NAME_UPPER_CAMEL}DTO dto) {
        ${TYPE_NAME_LOWER_CAMEL}Service.add${TYPE_NAME_UPPER_CAMEL}(dto);
    }

    /**
     * 分页获取${DOMAIN_DESC}列表
     *
     * @param dto {@link ${TYPE_NAME_UPPER_CAMEL}PageDTO}
     * @return {@link ${TYPE_NAME_UPPER_CAMEL}PageVO}s
     */
    @Operation(summary = "分页获取${DOMAIN_DESC}列表")
    @GetMapping(value = "/page")
    public Page<${TYPE_NAME_UPPER_CAMEL}PageVO> list${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}PageDTO dto) {
        return ${TYPE_NAME_LOWER_CAMEL}Service.list${TYPE_NAME_UPPER_CAMEL}(dto);
    }

    /**
     * 根据 id 获取${DOMAIN_DESC}
     *
     * @param id id
     * @return {@link ${TYPE_NAME_UPPER_CAMEL}VO}
     */
    @Operation(summary = "根据 id 获取 ${DOMAIN_DESC}")
    @GetMapping(value = "/{id}")
    public ${TYPE_NAME_UPPER_CAMEL}VO get${TYPE_NAME_UPPER_CAMEL}ById(@PathVariable("id") Long id) {
        return ${TYPE_NAME_LOWER_CAMEL}Service.get${TYPE_NAME_UPPER_CAMEL}ById(id);
    }

    /**
     * 根据 id 修改${DOMAIN_DESC}
     *
     * @param id  id
     * @param dto {@link ${TYPE_NAME_UPPER_CAMEL}DTO}
     */
    @Operation(summary = "修改${DOMAIN_DESC}")
    @PutMapping(value = "/{id}")
    public void update${TYPE_NAME_UPPER_CAMEL}ById(@PathVariable("id") Long id, @Validated @RequestBody ${TYPE_NAME_UPPER_CAMEL}DTO dto) {
        ${TYPE_NAME_LOWER_CAMEL}Service.update${TYPE_NAME_UPPER_CAMEL}ById(id, dto);
    }

    /**
     * 根据 id 删除${DOMAIN_DESC}
     *
     * @param id id
     */
    @Operation(summary = "根据 id 删除${DOMAIN_DESC}")
    @DeleteMapping(value = "/{id}")
    public void delete${TYPE_NAME_UPPER_CAMEL}ById(@PathVariable("id") Long id) {
        ${TYPE_NAME_LOWER_CAMEL}Service.delete${TYPE_NAME_UPPER_CAMEL}ById(id);
    }
}