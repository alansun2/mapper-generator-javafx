<#macro ignoreColumnCheck className name name1 suffixName1 suffixName2 items ignoreItemsMap needNew>
<#--    判断needNew 为 null，如果为 null，默认值为 true-->
        <#assign needNew = needNew!true>
        <#if needNew>
        final ${className} ${name} = new ${className}();
        </#if>
    <#assign ignoreItems1 = ignoreItemsMap[suffixName1]!>
    <#assign ignoreItems2 = ignoreItemsMap[suffixName2]!>
    <#list items as item>
        <#if ignoreItems1?seq_contains(item?lower_case) == false && ignoreItems2?seq_contains(item?lower_case) == false>
        ${name}.set${item}(${name1}.get${item}());
        </#if>
    </#list>
</#macro>

<#macro getPackage suffixs packageMap>
    <#list suffixs as suffix>
        <#assign key = 'PACKAGE_' + suffix>
        <#assign value = packageMap[key]!>
import ${value};
    </#list>
</#macro>