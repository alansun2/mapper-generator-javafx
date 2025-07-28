package ${PACKAGE};

import com.mybatisflex.core.paginate.Page;
import org.springframework.beans.BeanUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.beans.factory.annotation.Autowired;
<#assign suffixs = ["", "Service", "VO", "DTO"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>
import org.springframework.web.bind.annotation.RestController;
import java.util.List;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
@RestController
@RequestMapping("/${TYPE_NAME_LOWER_HYPHEN}")
public class ${TYPE_NAME_UPPER_CAMEL}${CLASS_SUFFIX} {

    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Service ${TYPE_NAME_LOWER_CAMEL}Service;

    /**
     * 保存。
     *
     * @param dto
     * @return {@code true} 保存成功，{@code false} 保存失败
     */
    @PostMapping("save")
    public boolean save(@Validated @RequestBody ${TYPE_NAME_UPPER_CAMEL}DTO dto) {
        final ${TYPE_NAME_UPPER_CAMEL} do = new ${TYPE_NAME_UPPER_CAMEL}();
        BeanUtils.copyProperties(dto, do);
        return ${TYPE_NAME_LOWER_CAMEL}Service.save(do);
    }

    /**
     * 根据主键删除。
     *
     * @param id 主键
     * @return {@code true} 删除成功，{@code false} 删除失败
     */
    @DeleteMapping("remove/{id}")
    public boolean remove(@PathVariable Long id) {
        return ${TYPE_NAME_LOWER_CAMEL}Service.removeById(id);
    }

    /**
     * 根据主键更新。
     *
     * @param do
     * @return {@code true} 更新成功，{@code false} 更新失败
     */
    @PutMapping("update")
    public boolean update(@Validated @RequestBody ${TYPE_NAME_UPPER_CAMEL}DTO dto) {
        final ${TYPE_NAME_UPPER_CAMEL} do = new ${TYPE_NAME_UPPER_CAMEL}();
        BeanUtils.copyProperties(dto, do);
        return ${TYPE_NAME_LOWER_CAMEL}Service.updateById(do);
    }

    /**
     * 查询所有。
     *
     * @return 所有数据
     */
    @GetMapping("list")
    public List<${TYPE_NAME_UPPER_CAMEL}VO> list() {
        return ${TYPE_NAME_LOWER_CAMEL}Service.listAll().stream()
                .map(do -> {
                    ${TYPE_NAME_UPPER_CAMEL}VO vo = new ${TYPE_NAME_UPPER_CAMEL}VO();
                    BeanUtils.copyProperties(do, vo);
                    return vo;
                })
                .collect(Collectors.toList());
    }

    /**
     * 根据主键获取。
     *
     * @param id 主键
     * @return 详情
     */
    @GetMapping("getInfo/{id}")
    public ${TYPE_NAME_UPPER_CAMEL}VO getInfo(@PathVariable Long id) {
        final ${TYPE_NAME_UPPER_CAMEL}VO vo = new ${TYPE_NAME_UPPER_CAMEL}VO();
        final ${TYPE_NAME_UPPER_CAMEL} do = ${TYPE_NAME_LOWER_CAMEL}Service.getById(id);
        BeanUtils.copyProperties(do, vo);
        return vo;
    }

    /**
     * 分页查询。
     *
     * @param page 分页对象
     * @return 分页对象
     */
    @GetMapping("page")
    public Page<${TYPE_NAME_LOWER_CAMEL}VO> page(Page<${TYPE_NAME_LOWER_CAMEL}> page) {
        return ${TYPE_NAME_LOWER_CAMEL}Service.page(page)
            .map(do -> {
                final ${TYPE_NAME_UPPER_CAMEL}VO vo = new ${TYPE_NAME_UPPER_CAMEL}VO();
                BeanUtils.copyProperties(do, vo);
                return vo;
            });
    }

}
