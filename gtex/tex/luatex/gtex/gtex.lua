function guixref(id)
   local m = " guix:" .. id
   local mac = token.get_macro(m)
   if (mac == nil) then
      tex.print(m .. "??")
   else
      tex.print(mac)
   end
end

function load_guixrefs(path)
   local inputs_file = io.open(path, "r")
   if (inputs_file == nil) then
      tex.print("\\opwarning{The auxiliary file " .. path .. " does not exist yet, please run gtex to create it}")
   else
      local s = inputs_file:read("*a")
      for k, v in s:gmatch("([^:]+):([^\n]*)\n") do
         token.set_macro(" guix:" .. k, v)
      end
   end
end

function escape_code(s)
   return s:gsub("\\", "\\\\"):gsub("\"", "\\\""):gsub("\n", "\\n")
end

gscriptf = nil
function guix(id, expr)
   -- Only open if actually used
   if (gscriptf == nil) then
      gscriptf = io.open(tex.jobname .. ".scm", "w")
   end
   gscriptf:write(id .. ":" .. expr .. "\n")
end

reading_code = false
reading_code_buffer = ""
reading_code_spec = ""
reading_code_idx = 0
function read_code(s)
   if (s == "") then
      reading_code_buffer = reading_code_buffer .. "\n"
      return s
   end
   first = utf8.codepoint(s)
   if reading_code then
      if (first == utf8.codepoint('»')) then
         reading_code = false
         ids = "code.src" .. reading_code_idx
         idr = "code.res" .. reading_code_idx
         src = "((@ (guix gexp) mixed-text-file) \"" .. ids ..
            "\" \"" .. escape_code(reading_code_buffer) .. "\" )"
         guix(ids, src)
         guix(idr,
              "((@ (gtex) eval-script) \"" .. reading_code_spec
              .. "\" " .. src .. ")")
         out = "\\vfil{\\verbinput(-) \\guixref{" .. ids .. "} \\relax\\medskip\\hrule\\medskip\\verbinput(-) \\guixref{" .. idr .. "} \\relax}\\vfil"
         return out
      end
      if (reading_code_buffer == "") then
         reading_code_buffer = s
      else
         reading_code_buffer = reading_code_buffer .. "\n" .. s
      end
      return ""
   else
      if (first == utf8.codepoint('«')) then
         reading_code = true
         reading_code_buffer = ""
         reading_code_idx = reading_code_idx + 1
         reading_code_spec = s:sub(utf8.offset(s, 2))
         return ""
      end
      -- Don't touch, returning nil breaks LuaTeX or OpTeX somewhere
      return s
   end
end

callback.add_to_callback("process_input_buffer", read_code, "read_code")
