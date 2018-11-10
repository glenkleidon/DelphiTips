unit PropertyBagInterfaces;

interface
{$IFDEF MSWINDOWS}
uses Messages, Windows, ActiveX;
{$ELSE}
uses Messages, Types, Windows;
{$ENDIF}


 type
{ IPropertyBag2 interface }
  tagPROPBAG2 = record
    dwType: DWORD;
    vt: TVarType;
    cfType: TClipFormat;
    dwHint: DWORD;
    pstrName: POleStr;
    clsid: TCLSID;
  end;
  {$EXTERNALSYM tagPROPBAG2}
  TPropBag2 = tagPROPBAG2;
  PPropBag2 = ^TPropBag2;

  IPropertyBag2 = interface(IUnknown)
    ['{22F55882-280B-11d0-A8A9-00A0C90C2004}']
    function Read(pPropBag: PPropBag2; pErrLog: IErrorLog;
      pvarValue: PVariant; phrError: PHResult): HRESULT; stdcall;

    function Write(cProperties: ULONG; pPropBag: PPropBag2;
      pvarValue: PVariant): HRESULT; stdcall;
    function CountProperties(var pcProperties: ULONG): HRESULT; stdcall;

    function GetPropertyInfo(iProperty, cProperties: ULONG;
      pPropBag: PPropBag2; var pcProperties: ULONG): HRESULT; stdcall;

    function LoadObject(pstrName:POleStr; dwHint: DWORD; pUnkObject: IUnknown;
      pErrLog: IErrorLog): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IPropertyBag2}

implementation

end.
